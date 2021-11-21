import { select, selectAll } from 'd3-selection'
import { scaleOrdinal } from 'd3-scale'
import { timer } from 'd3-timer'
import { interpolateZoom } from 'd3-interpolate'
import { easePolyOut, easePolyInOut } from 'd3-ease'
import { hierarchy, pack } from 'd3-hierarchy'
import { shadeColor, setpixelated, sleep } from './custom.js'

(function() {
    var requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame ||
        window.webkitRequestAnimationFrame || window.msRequestAnimationFrame;
    window.requestAnimationFrame = requestAnimationFrame;
})();

const d3 = Object.assign(
	{},
	{
		select, selectAll,
		scaleOrdinal,
		timer,
		interpolateZoom,
		easePolyOut, easePolyInOut,
		hierarchy, pack
	},
)

const NodeType = {
    Circle: "Circle",
    Role: "Role",
}

const RoleType = {
    Owner: "Owner",
    Member: "Member",
    Guest: "Guest",
    Bot: "Bot",
    Retired: "Retired",
    Pending: "Pending",
    Coordinator: "Coordinator",
    Peer: "Peer",
}


// Flat list of nodes (unordered) to nested tree structure
// from: https://stackoverflow.com/questions/18017869/build-tree-array-from-flat-array-in-javascript/40732240#40732240
const formatGraph = dataset =>  {
    var dataTree = [];
    var dataDict = Object.create(null);

    dataset.forEach( (aData, i) => {
        dataDict[aData.nameid] = {
            ...aData,
            children : [],
            depth : 0
        }
    });

    dataset.forEach( aData => {
        // Filter Speciale Role nodes
        if (aData.role_type == RoleType.Member || aData.role_type == RoleType.Owner || aData.role_type == RoleType.Bot ) {
            delete dataDict[aData.nameid]
            return
        }

        if(aData.parent) {
            dataDict[aData.parent.nameid].children.push(dataDict[aData.nameid])
        } else {
            dataTree.push(dataDict[aData.nameid])
        }

    })
    return dataTree
}

// Recursively traverse the graph and add to each nodes the attributes:
// * depth: depth position (startinf at 0)
// * neigbor: number of neogbor
// * cumchild: total number of child
// WARNING: @HACK: the improve the VX we add an invisible node
// for circle that have only one child.
const computeDepth = (obj, depth, neigbor) => {
    var maxdepth = 0;
    var cumchild = 0;
    if (depth === undefined) {
        var currentdepth = 0;
        var neigbor = 1;
    } else {
        var currentdepth = depth;
        neigbor = neigbor;
    }

    obj.depth = currentdepth;
    obj.neigbor = neigbor;

    if (obj.children) {
        if (obj.children.length == 1) {
            obj.children.push({
                type_: "Hidden",
            })
        }
        obj.children.forEach((d, i) =>  {
            var d = computeDepth(d, currentdepth+1, obj.children.length-1);
            var tmpDepth = d.maxdepth;
            cumchild += d.cumchild;
            if (tmpDepth > maxdepth) {
                maxdepth = tmpDepth;
            }
        });

    }
    maxdepth = maxdepth + 1;
    cumchild = cumchild + 1;
    obj.cumchild = cumchild;
    return {maxdepth, cumchild};
}


export const GraphPack = {

    // Background Colors
    //backgroundColor: "#f1fdff",
    //backgroundColor: "#f0fff0",
    //--
    backgroundColor: "#edfcff",
    //--
    //backgroundColor: "#3e4957",

    // Graph Colors
    //colorCircleRange: ['#d9d9d9','#838383','#4c4c4c','#1c1c1c', '#000000'],
    //colorCircleRange: ['#bfbfbf','#838383','#4c4c4c','#1c1c1c', '#000000'],
    colorCircleRange: [
        '#e0e0e0',
        '#b0b0b0',
        '#808080',
        '#505050',
        '#404040',
        '#303030',
        '#202020',
        '#101010',
        '#000000',
    ],
    usernameColor: "#8282cc",
    coordinatorRoleColor: "#ffdaa1", // ~orange
    peerRoleColor: "#edf5ff",// "#f0fff0", // "#FFFFF9"
    guestColor: "#f4fdf5", // ~yellow
    focusCircleColor: "#4a79ac", // blue>"#368ed3"
    hoverCircleColor: "#3f3f3fdd", //  grey-black>"#3f3f3f"
    hoverCircleWidth: 2,
    focusCircleWidth: 2*2.5, // warning, can break stroke with canvas drawing.

    // Html element ID
    canvasParentId: "canvasParent",
    canvasId: "canvasOrga",
    hiddenCanvasId: "hiddenCanvasOrga",

    // Geometry
    minWidth: 300,
    minHeight: 400,
    width: null,
    height: null,
    mobileSize: null,
    // Nodes/Circles geometry
    centerX: null,
    centerY: null,
    diameter: null,
    zoomCtx: null,
    circlesPadding: 8, // 1.8
    fontsizeCircle_start: 19,
    fontstyleCircle: "Arial",

    // Graph fx settings
    isLoading: true,
    minZoomDuration: 350, // 1250
    zoomFactorRoot: 2.02,
    zoomFactorCircle: 2.4,
    zoomFactorRole: 4,
    zoomFactorGuest: 5,
    // rayon size of the node in the canvas
    rayonFactorRole: 0.95,
    rayonFactorGuest: 0.75,
    guestSizeDivider: 7,
    // y-axis offset for the top node
    nodeOffsetY: 0,

    // State
    colToCircle : {}, // Dataset to swich between color of a circle (in the hidden canvas) and the node data
    nextCol: 1,
    colorCircle : null,
    rootNode    : null, // The root node of the graph
    focusedNode : null, // The node that has the active focus
    hoveredNode : null, // The node that is curently hoovered
    isFrozen    : false, // prevent mouse move glitching thing

    // Zooming
    ease: d3.easePolyInOut.exponent(4),
    //ease: d3.easePolyOut.exponent(4),
    isZooming: false,
    vpOld: null,

    // Resizing
    rtime: null,
    timeout: false,
    delta: 200,

    // Html Elements
    $nextToChart: null,
    $canvas: null,
    $hiddenCanvas: null,
    $canvasButtons: null,
    $tooltip: null,
    // Canvas ctx
    ctx2d: null,
    hiddenCtx2d: null,

    // Dat3
    gPack: null, // Receive D3 data structure
    gStats: null, // Receive graph global statistics
    nodes: null,  // List of D3 nodes
    nodesDict: null, // Nodes mapping
    app: null, // elm app
    uctx: null, // from localstorage

    /****************************************************/
    /*      Methods                                     */
    /****************************************************/

    //
    // Canvas drawing methods
    //

    // Reset drawing
    clearAll() {
        //this.$canvasButtons.classList.add("is-invisible");
        //this.$tooltip.classList.add("is-invisible");

        //if (this.$canvas) {
        //    this.$canvas.parentNode.removeChild(this.$canvas);
        //    //delete $canvas;
        //}
        //if (this.$hiddenCanvas)  {
        //    this.$hiddenCanvas.parentNode.removeChild(this.$hiddenCanvas);
        //    //delete $hiddenCanvas;
        //}
    },

    //Clear canvas
    clearCanvas(ctx2d) {
        //var backgoundGrd = ctx2d.createLinearGradient(0, 0, this.width, 0);
        //backgoundGrd.addColorStop(0, this.colorDarker1);
        //backgoundGrd.addColorStop(1, this.colorDarker2);
        ctx2d.fillStyle = this.backgroundColor;
        ctx2d.rect(0, 0, this.width, this.height);
        ctx2d.fill();
    },

    drawButtons() {
        var b = document.querySelector("body").getBoundingClientRect();
        var scrollLeft = b.left;
        var scrollTop = b.top;
        var r = this.$canvas.getBoundingClientRect();
        this.$canvasButtons.style.left = r.left + r.width - this.$canvasButtons.offsetWidth -8 -scrollLeft +"px";
        this.$canvasButtons.style.top = r.top + 13 -scrollTop +"px";

        this.$canvasButtons.classList.remove("is-invisible");
        this.$tooltip.classList.remove("is-invisible");
    },

    // Size the canvas
    computeGeometry() {
        this.computedWidth = this.$canvasParent.offsetWidth; //var computedWidth = parseInt(window.getComputedStyle($canvasParent).width, 10);
        this.computedHeight = (window.innerHeight)/2;

        // Canvas settings
        this.width = Math.max(this.computedWidth, this.minWidth);
        this.height = Math.max(this.computedHeight, this.minHeight); //(computedHeight > computedWidth ?  computedWidth: computedHeight );
        this.mobileSize = (window.innerWidth < 768 ? true : false);

        this.diameter = Math.min(this.width*0.97, this.height*0.97);
        this.centerX = this.width/2;
        this.centerY = this.height/2;
        this.zoomCtx = {
            // Init at CenterX, centerY
            centerX: this.centerX,
            centerY: this.centerY,
            scale: 1
        };
    },

    // Resize Html Elements created here
    sizeDom() {
        // Size Canvas
        this.$canvas.width = this.width;
        this.$canvas.height = this.height;
        if (this.$hiddenCanvas) {
            this.$hiddenCanvas.width = this.width;
            this.$hiddenCanvas.height = this.height;
        }

        // Size Element next to the canvas
        this.$nextToChart.style.minHeight = 1.5*this.height+"px";
    },

    //The draw function of the canvas that gets called on each frame
    drawCanvas(isHidden) {
        var ctx2d;
        if (isHidden) {
            ctx2d = this.hiddenCtx2d;
        } else {
            ctx2d = this.ctx2d;
        }
        this.clearCanvas(ctx2d);

        //Select our dummy nodes and draw the data to canvas.
        var focus = this.focusedNode;
        var node;
        // It's slightly faster than nodes.forEach()
        for (var i = 0; i < this.nodes.length; i++) {
            node = this.nodes[i];
            let d = node.depth - focus.depth
            if (node == focus.parent || node.depth == 0 || d>= 0 && d < 4) {
                this.drawNode(isHidden, ctx2d, node);
            }
        }

        if (!isHidden) {
            this.drawCircleNames(focus)
        }

    },

    drawNode(isHidden, ctx2d, node) {
        var _name = node.data.name;
        var type_ = node.data.type_;
        var role_type = node.data.role_type;
        var circleColor = "#";

        if (type_ === "Hidden") {
            return
        } else {
            this.addNodeCtx(node);
        }

        // If the hidden canvas was send into this function and it does not yet have a color,
        // generate a unique one.
        if(isHidden) {
            if(node.color === undefined) {
                // If we have never drawn the node to the hidden canvas get a new color for it and put it in the dictionary.
                node.color = this.genColor();
                this.colToCircle[node.color] = node;
            }
            // On the hidden canvas each rectangle gets a unique color.
            circleColor = node.color;
        } else {
            var grd = ctx2d.createRadialGradient(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon*0.1, node.ctx.centerX, node.ctx.centerY, node.ctx.rayon*2);
            // opacity level
            var opac = "";
            // @DEBUG: compute this only one time when choosing the focusedNode
            if (node !== this.rootNode && !this.gPack.path(node).map(n => n.data.nameid).includes(this.focusedNode.data.nameid)) {
                opac = "55";
            }
            if (type_ === NodeType.Circle) {
                grd.addColorStop(0.25, this.colorCircle(node.depth)+opac);
                grd.addColorStop(1, this.colorCircle(node.depth+1)+opac);
            } else if (type_ === NodeType.Role) {
                // Check role type code color
                if (role_type === RoleType.Guest) {
                    circleColor = this.guestColor;
                } else if (role_type == RoleType.Coordinator) {
                    circleColor = this.coordinatorRoleColor;
                } else {
                    circleColor = this.peerRoleColor;
                }
                grd.addColorStop(0, circleColor+opac);
                grd.addColorStop(1, shadeColor(circleColor, -5)+opac);
            } else {
                console.warn("Node type unknonw", type_);
            }

            circleColor = grd;
        }

        // Draw circle
        ctx2d.beginPath();
        ctx2d.fillStyle = circleColor;
        ctx2d.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon,
            0, 2 * Math.PI, true);
        ctx2d.fill();


        // If role is own by user, enlight the circle
        if ( (this.uctx && node.data.first_link) && type_ === NodeType.Role && this.uctx.username == node.data.first_link.username ) {
            // Draw user pin
            //var r =  Math.max(10 - (node.depth - this.focusedNode.depth) , 1)/4
            //ctx2d.beginPath();
            //ctx2d.fillStyle = "green";
            //ctx2d.arc(node.ctx.centerX, node.ctx.centerY + node.ctx.rayon*3/4 , r,
            //    0, 2 * Math.PI, true);
            //ctx2d.fill();

            // Draw user border
            var hoverWidth = 2;
            var hoverColor = "green";
            ctx2d.beginPath();
            ctx2d.setLineDash([5, 5]);
            ctx2d.beginPath();
            ctx2d.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon-hoverWidth*0.5,
                0, 2 * Math.PI, true);
            ctx2d.lineWidth   = hoverWidth;
            ctx2d.strokeStyle = hoverColor;
            ctx2d.stroke();
            ctx2d.setLineDash([]);
        }

        if (!isHidden) {
            if (node === this.focusedNode) {
                // Draw focused border
                var hoverWidth = this.focusCircleWidth;
                var hoverColor = this.focusCircleColor;
                // Draw border
                ctx2d.beginPath();
                ctx2d.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon+0.1+hoverWidth*0.5,
                    0, 2 * Math.PI, true);
                ctx2d.lineWidth   = hoverWidth;
                ctx2d.strokeStyle = hoverColor;
                ctx2d.stroke();
            }

            var text = null;
            var user = null;
            var fontSize = this.fontsizeCircle_start;
            if (type_ === NodeType.Role && opac === "") {
                // Show Role name
                var textLong = _name;
                var textShort = _name.substring(0, 3);
                //var textShort = _name.substring(0, 3).replace(/./,x=>x.toUpperCase());
                if (node.data.first_link) {
                    user = "@"+node.data.first_link.username;
                }

                // Name
                ctx2d.font = fontSize + "px " + this.fontstyleCircle;
                var textWidth = ctx2d.measureText(textLong).width;
                var textHeight = fontSize/3;
                var paddingBelow = 0;
                if (textWidth+textHeight < node.ctx.rayon*2) {
                    text = textLong;
                } else if (ctx2d.measureText(textShort).width+5 < node.ctx.rayon*2) {
                    text = textShort;
                } else {
                    fontSize = fontSize - 2;
                }

                // Username
                var text_username = null;
                var text_username_short = "@";
                if (user && ctx2d.measureText(user).width+1 < node.ctx.rayon*2 - 2*textHeight) {
                    text_username = user;
                    paddingBelow = 4*textHeight;
                } else if (user && ctx2d.measureText(text_username_short).width+5 < node.ctx.rayon*2 - 2*textHeight) {
                    text_username = text_username_short;
                    paddingBelow = 3*textHeight;
                }

                if (text) {
                    ctx2d.beginPath();
                    ctx2d.fillStyle = "black";
                    ctx2d.textAlign = "center";
                    ctx2d.fillText(text, node.ctx.centerX, node.ctx.centerY+textHeight);
                    //ctx2d.shadowColor = '#999'; //ctx2d.shadowBlur = 10; //ctx2d.shadowOffsetX = 1; //ctx2d.shadowOffsetY = 1;
                    ctx2d.fill();

                    if (text_username) {
                        ctx2d.font = fontSize-7 + "px " + this.fontstyleCircle;
                        ctx2d.beginPath();
                        ctx2d.fillStyle = this.usernameColor;
                        ctx2d.fillText(text_username, node.ctx.centerX, node.ctx.centerY + paddingBelow);
                        ctx2d.fill();
                    }
                }


            } else {
                //if (this.focusedNode.depth == node.depth || this.focusedNode.depth == node.depth-1 ) {
                //    ctx2d.beginPath();
                //    ctx2d.fillStyle = "white";
                //    ctx2d.fillCircleText(_name,
                //        node.ctx.centerX, node.ctx.centerY,
                //        node.ctx.rayon, -Math.PI*0.7);
                //    ctx2d.fill()
                //}
            }
        }
    },

    // Create the interpolation function between current view and the clicked on node.
    // It firsts zoom to get the circles to the right location
    // then timer the interpolateZoom and rendering.
    // If `delay` is given, it overwrite the zoom duration. Give a low value for flush reset.
    zoomToNode(focus, delay) {
        //Based on the generous help by Stephan Smola
        //http://bl.ocks.org/smoli/d7e4f9199c15d71258b5
        if (this.isZooming) {
            return false
        }

        if (focus && typeof(focus) === 'string') {
            var maybeFocus = this.nodesDict[unescape(focus)];
            if (!maybeFocus) {
                console.warn("Unknown node:", focus);
                console.warn("Redirecting to root");

                maybeFocus = this.setFocus(this.rootNode);
                //delay = 0.5;
            }
            focus = maybeFocus;
        } else { // assume node
            // pass
        }

        var elmHasBeenUpdated = false;
        if (this.focusedNode.ctx) {
            this.clearNodeHover(this.focusedNode);
            this.nodeFocusedFromJs(focus);
            elmHasBeenUpdated = true;
        }
        var oldFocus = this.focusedNode;
        this.focusedNode = focus;
        this.drawNodeHover(this.focusedNode, false);

        var zoomFactor = this.getZoomFactor(this.focusedNode);

        // Configre interpolator
        var vp = [this.focusedNode.x, this.focusedNode.y, this.focusedNode.r * zoomFactor]; //The center and width of the new "viewport"
        delay = (delay === undefined ? 0 : delay*this.minZoomDuration);
        var maxDuration = this.minZoomDuration*2;
        var interpolator = d3.interpolateZoom(this.vpOld, vp); //Create interpolation between current and new "viewport"
        var duration = Math.min(interpolator.duration, maxDuration) || delay; //Interpolation gives back a suggested duration
        duration = (duration < 0) ?  maxDuration : duration;
        var timeElapsed = 0+delay; //Set the time elapsed for the interpolateZoom function to 0
        //console.log("old", this.vpOld, "new", vp, "delay", delay)
        this.vpOld = vp; //Save the "viewport" of the next state as the next "old" state

        //Perform the interpolation and continuously change the zoomCtx while the "transition" occurs.
        var interpolateZoom = (dt) => {
            if (duration) {
                timeElapsed += dt;
                var t = this.ease(timeElapsed / duration);

                this.zoomCtx.centerX = interpolator(t)[0];
                this.zoomCtx.centerY = interpolator(t)[1];
                this.zoomCtx.scale = this.diameter / interpolator(t)[2];

                if (timeElapsed >= duration) {
                    return true
                } else {
                    return false
                }
            }
            // do no stay lock here
            return true
        };

        var	dt = 0;
        var t = d3.timer((elapsed) => {
            //stats.begin();
            this.isZooming = true;
            var finished = interpolateZoom(elapsed - dt);
            dt = elapsed;
            this.drawCanvas();
            //stats.end();
            if (finished) {
                this.isZooming = false;
                this.drawCanvas();
                this.drawCanvas(true);
                if (!elmHasBeenUpdated) this.nodeFocusedFromJs(this.focusedNode); // INIT
                this.drawNodeHover(this.focusedNode, true);
                t.stop();
            }
        });

    },

    //
    // D3/GraphPack
    //

    // Determine the node size in the circle packing
    // Returns: int f(n.depth, n.neigbor, n.cumchild)
    nodeSizeTopDown3(n, stats) {
        var t = 0;
        var rt = 0;
        if (n.type_ == "Circle") {
            t = 1;
        } else if (n.type_ == "Role") {
            t = 2;
        } else {
            t = 1;
        }

        var dvd = (n.role_type == RoleType.Guest) ? this.guestSizeDivider : 1;
        var v = t+rt;
        return Math.log(1/n.depth**v+1) / dvd
    },
    nodeSizeTopDown(n, stats) {
        var dvd = (n.role_type == RoleType.Guest) ? this.guestSizeDivider : 1;
        var t = 0;
        if (n.type_ == "Circle") {
            t = 4;
        } else if (n.type_ == "Role") {
            t = 2;
        } else {
            t = 1;
        }
        var v = t;
        return v*100000 / (n.depth+1)**(3) / dvd
    },
    nodeSizeTopDown_orig(n, stats) {
        var dvd = (n.role_type == RoleType.Guest) ? this.guestSizeDivider : 1;
        return 10000/(stats.maxdepth)**(Math.max(1.5, n.depth)) / dvd
    },

    nodeSizeBottomUp(n, stats) {
        var dvd = (n.role_type == RoleType.Guest) ? this.guestSizeDivider : 1;
        var sizeDefault = 4;
        return 10000/(stats.maxdepth)**(Math.max(0, sizeDefault - n.depth)) / dvd
    },

    // Mapping function from a node depth to color.
    colorCircle(k) {
        //d3.scaleOrdinal()
        //.domain(Array.from({length:this.colorCircleRange.length},(v,k)=>k%this.colorCircleRange.length))
        //.range(this.colorCircleRange)
        //.unknown(this.backgroundColor);
        return this.colorCircleRange[k%this.colorCircleRange.length]
    },

    getZoomFactor(node) {
        var zoomFactor;
        if (node.data.type_ === NodeType.Role) {
            if (node.data.role_type == "Guest") {
                zoomFactor = this.zoomFactorGuest;
            } else {
                zoomFactor = this.zoomFactorRole;
            }
        } else if (node.data.parent == undefined ) {
            zoomFactor = this.zoomFactorRoot;
        } else if (node.children && node.children.length >= 10 ) {
            zoomFactor = this.zoomFactorRoot;
        } else {
            zoomFactor = this.zoomFactorCircle;
        }
        return zoomFactor
    },

    // Init and create the GraphPack data structure
    resetGraphPack(dataNodes, doFormat, focusid) {
        var graph;
        if (doFormat) {
            if (dataNodes.length == 0) {
                console.warn("Graph is empty, aborting");
                return
            }
            graph = formatGraph(dataNodes);
            if (graph.length > 1) console.warn("More than 1 graph given -> Some nodes are not connected.");
            else graph = graph[0];
        }
        else
            graph = dataNodes;

        // Determine the node order in the circle packing
        const nodeOrder = (n1, n2) => {
            // node order
            return n1.data.createdAt > n2.data.createdAt
        }

        // Compute global statistics
        this.gStats = computeDepth(graph);

        // Compute circle packing
        this.gPack = d3.pack()
            .padding(this.circlesPadding)
            .size([this.diameter, this.diameter])
        (d3.hierarchy(graph)
            .sum(d => this.nodeSize(d, this.gStats))
            .sort(nodeOrder));

        this.nodesDict = Object.create(null);
        this.nodes = this.gPack.descendants(graph);
        this.rootNode = this.nodes[0];
        this.hoveredNode = null;
        this.nodes.forEach(n => this.nodesDict[n.data.nameid] = n);
        this.graph = graph;
        this.setFocus(focusid, true);

        this.uctx = JSON.parse(localStorage.getItem("user_ctx"))
    },

    setFocus(n, setViewport) {
        if (!n) {
            // focus on root node by default
            this.focusedNode = this.rootNode;
        } else if (typeof(n) === 'string') {
            // Whit it doesnt works ?
            //this.focusedNode = this.nodes.find(n => {n.data.nameid === n });
            this.focusedNode = this.nodesDict[n];
        } else {
            // assume node
            this.focusedNode = n;
        }

        if (setViewport) {
            this.vpOld = [this.focusedNode.x, this.focusedNode.y, this.focusedNode.r * this.getZoomFactor(this.focusedNode)];
        }

        return this.focusedNode

    },

    //
    // Utils Methods
    //

    //Generates the next color in the sequence, going from 0,0,0 to 255,255,255.
    //From: https://bocoup.com/weblog/2d-picking-in-canvas
    genColor() {
        var ret = [];
        // via http://stackoverflow.com/a/15804183
        if(this.nextCol < 16777215) {
            ret.push(this.nextCol & 0xff); // R
            ret.push((this.nextCol & 0xff00) >> 8); // G
            ret.push((this.nextCol & 0xff0000) >> 16); // B

            this.nextCol += 100; // This is exagerated for this example and would ordinarily be 1.
        }
        var col = "rgb(" + ret.join(',') + ")";
        return col;
    },

    // Get the mouse coordinate whithin the canvas reference.
    getPointerCtx(e) {
        var r = this.$canvas.getBoundingClientRect();
        var mouseX = (e.clientX - r.left);
        var mouseY = (e.clientY - r.top);
        return {mouseX, mouseY}
    },


    // Returns a PNode from a Node
    getPNode(node) {
        return {
            nameid: node.data.nameid,
            name: node.data.name,
            charac: node.data.charac,
        }
    },

    // Returns the path from root to node.
    getNodePath(node) {
        var path = this.gPack.path(node).map(n => {
            return this.getPNode(n)
        });
        return path
    },

    getNodeData(node) {
        // @debug: LocalGraph/Node Encoder/Decoder
        var rootNode = this.getPNode(this.rootNode);
        var focusNode = {
            name: node.data.name,
            nameid: node.data.nameid,
            type_: node.data.type_,
            charac: node.data.charac,
            children: (node.children) ? node.children.filter(n => n.data.type_ !== "Hidden").map(n => {
                return {
                    name: n.data.name,
                    nameid: n.data.nameid,
                    role_type: n.data.role_type,
                    charac: n.data.charac,
                }
            }) : [],
            source: node.data.source,
        };

        return {
            root: rootNode,
            path: this.getNodePath(node),
            focus: focusNode
        }
    },

    // Get node position and properties
    addNodeCtx(node) {
        var zoomCtx = this.zoomCtx;
        var ctx, centerX, centerY, rayon;

        centerX = ((node.x - zoomCtx.centerX) * zoomCtx.scale) + this.centerX;
        centerY = ((node.y - zoomCtx.centerY) * zoomCtx.scale) + this.centerY + this.nodeOffsetY;
        if (node.data.type_ === NodeType.Role) {
            if (node.data.role_type === RoleType.Guest) {
                rayon = node.r * this.rayonFactorGuest;
            } else {
                // Regular member
                rayon = node.r * this.rayonFactorRole;
            }
        } else {
            // Circle
            rayon = node.r;
        }
        rayon *= (zoomCtx.scale);
        //rayon *= (zoomCtx.scale + node.depth*0.1);
        node.ctx = {centerX, centerY, rayon};
        return
    },

    // Get the node under cursor in the canvas
    getNodeUnderPointer(e) {
        //Figure out where the mouse click occurred.
        var p = this.getPointerCtx(e);
        var hiddenCtx2d = this.hiddenCtx2d;

        // Get the corresponding pixel color on the hidden canvas and look up the node in our map.
        // This will return that pixel's color
        var pixel = hiddenCtx2d.getImageData(p.mouseX, p.mouseY, 1, 1).data;
        //Our map uses these rgb strings as keys to nodes.
        var color = "rgb(" + pixel[0] + "," + pixel[1] + ","+ pixel[2] + ")";
        var node = this.colToCircle[color];
        if (node) {
            this.addNodeCtx(node);
        }
        return node;
    },

    drawCircleNames(node) {
        var ctx2d = this.ctx2d
        var n;
        for (var i=0; i < node.data.children.length; i++) {
            n = node.children[i];
            if (!n.ctx) continue
            var fontSize = this.fontsizeCircle_start;
            var text, textWidth , textHeight;

            if (n.data.type_ === NodeType.Circle && node.depth == n.depth-1 ) {
                // Show Circle name
                text = n.data.name;
                textWidth = ctx2d.measureText(text).width;
                textHeight = fontSize/3;
                if (textWidth-textHeight > (n.ctx.rayon)*2.5) {
                    text = text.split(" ").map(s => s.substring(0, 3)).join("·")
                    textWidth = ctx2d.measureText(text).width;
                    if (textWidth-textHeight > n.ctx.rayon*2.5) {
                        text = text.split("·").map(s => s.substring(0, 1)).join("·")
                    }
                } else {
                    //text = text
                }

                fontSize = this.fontsizeCircle_start;
                ctx2d.font = "bold " + (fontSize) + "px " + this.fontstyleCircle;
                //ctx2d.font = "bold " + fontSize + "px " + "Courier New"; // serif, sans-serif, Verdana, Georgia
                ctx2d.beginPath();
                ctx2d.textAlign = "center";
                ctx2d.fillStyle = "#172335ff";
                ctx2d.fillText(text, n.ctx.centerX, n.ctx.centerY-n.ctx.rayon/2+textHeight);
                //ctx2d.lineWidth = 0.2;
                //ctx2d.strokeStyle = "black";
                //ctx2d.strokeText(text, n.ctx.centerX, n.ctx.centerY-n.ctx.rayon/2+textHeight);
                //ctx2d.shadowColor = '#999'; //ctx2d.shadowBlur = 10; //ctx2d.shadowOffsetX = 1; //ctx2d.shadowOffsetY = 1;
                ctx2d.fill();
                ctx2d.stroke();
            }
        }
    },

    // Draw node border + eventually tooltip
    drawNodeHover(node, doDrawTooltip) {
        var ctx2d = this.ctx2d;
        if (!node.ctx) {
            // Wait for the canvas to render before drawing border.
            // If not, focus border won be draw if another circle in hover before rendering.
            return false
        }

        // Clear Border
        var clearBorder = this.hoveredNode && (this.hoveredNode != this.focusedNode);
        if (clearBorder) {
            this.clearNodeHover(this.hoveredNode);
        }

        // Draw Border (on hoover)
        if (node != this.hoveredNode && node != this.focusedNode) {
            var hoverColor,
                hoverWidth,
                offset_r = 0;
            if (node == this.focusedNode) {
                hoverColor = this.focusCircleColor;
                hoverWidth = this.focusCircleWidth;
            } else {
                hoverColor = this.hoverCircleColor;
                hoverWidth = this.hoverCircleWidth;
                if (node.data.type_ == NodeType.Circle) offset_r = -0.5;
                else if (node.data.type_ == NodeType.Role) offset_r = 0.1;
            }

            // Draw Circle border
            ctx2d.beginPath();
            ctx2d.arc(node.ctx.centerX, node.ctx.centerY,
                node.ctx.rayon+offset_r+hoverWidth*0.5, 0, 2 * Math.PI, true);
            ctx2d.lineWidth = hoverWidth;
            ctx2d.strokeStyle = hoverColor;
            ctx2d.stroke();
        }

        // Draw tooltip
        if (doDrawTooltip) {
            this.drawNodeTooltip(node);
        }

        // Update global context
        this.hoveredNode = node; //@debug: use globCtx

        // rewrite circle child name
        this.drawCircleNames(this.focusedNode);
        return
    },

    // Clean node hovering
    clearNodeHover(node) {
        var ctx2d = this.ctx2d;
        //if (!node.ctx) {
        //    this.addNodeCtx(node)
        //}

        var hoverWidth;
        if (node == this.focusedNode) {
            hoverWidth = this.focusCircleWidth;
        } else {
            hoverWidth = this.hoverCircleWidth;
        }

        // cant get the original colors !?!
        //var pixel = ctx2d.getImageData(node.ctx.centerX+node.r+hoverWidth*30, node.ctx.centerY, 1, 1).data;
        //var color = "rgb(" + pixel[0] + "," + pixel[1] + ","+ pixel[2] + ","+ pixel[3] + ")";

        // Clear Circle Border
        ctx2d.beginPath();
        ctx2d.arc(node.ctx.centerX, node.ctx.centerY,
            node.ctx.rayon+0.1+hoverWidth/2, 0, 2 * Math.PI, true);
        ctx2d.lineWidth = hoverWidth*1.75;
        ctx2d.strokeStyle = ((node.depth == 0)? this.backgroundColor : this.colorCircle(node.depth-1));
        ctx2d.stroke();

        // Clear node tooltip
        this.clearNodeTooltip();

        // Update context
        this.hoveredNode = null; //@debug: use globCtx
        return
    },

    // Draw the node tooltip
    drawNodeTooltip(node) {
        this.nodeHoveredFromJs(node);
        // Add a timer, to wait the nodeHover elm render the toolip options.
        // elm code.
        setTimeout(() => {
            this.drawNodeTooltip_(node);
        }, 50);
    },
    drawNodeTooltip_(node) {
        var $tooltip = this.$tooltip
        // == add tooltip
        // @DEBUG: tooltip neeed to be displayed to get its clientWidth.
        var $subTooltip = document.getElementById(this.$tooltip.dataset.eventClick);
        if (!$subTooltip) return
        $subTooltip.childNodes[0].textContent = node.data.name;
        $tooltip.classList.remove("fadeOut");
        $tooltip.classList.add("fadeIn");
        // --
        var bodyRect = document.querySelector("body").getBoundingClientRect();
        var scrollLeft = bodyRect.left;
        var scrollTop = bodyRect.top;
        var r = this.$canvas.getBoundingClientRect();
        var tw = $tooltip.clientWidth;
        var l = (node.ctx.centerX + r.left - scrollLeft - (tw/2 + 1));
        if (node == this.focusedNode) {
            // below the circle
            var hw = (-$tooltip.clientHeight + 2*node.ctx.rayon);
            var t = (node.ctx.centerY + r.top  - scrollTop  - (hw/2 + 23));
        } else {
            // above the circle
            var hw = ($tooltip.clientHeight + 2*node.ctx.rayon);
            var t = (node.ctx.centerY + r.top  - scrollTop  - (hw/2 + 23));
        }

        if (l+tw/2-r.left < 0 || r.left+r.width-tw/2-l < 0 ) {
            // the tooltip overflow "too much" outside the canvas. (left/right
            this.clearNodeTooltip();
            return
        } else if ( t+$tooltip.clientHeight/3-r.top < 0) {
            // Overflow on top
            var hw = (-$tooltip.clientHeight/2 + 2*node.ctx.rayon);
            var t = (node.ctx.centerY + r.top  - scrollTop  - (hw/2 + 23));
        }
        $tooltip.style.left = l + "px";
        $tooltip.style.top = t + "px";

        return
    },

    // Clear node tooltip.
    clearNodeTooltip() {
        if (this.$tooltip) {
            this.$tooltip.classList.remove("fadeIn");
            this.$tooltip.classList.add("fadeOut");
            //this.$tooltip.style.display = "none";
        }

        this.nodeHoveredFromJs("");
        return
    },

    // check geometrical condition
    checkIf(p, cond, nodeOrElt) {
        var test;
        switch(cond) {
            case 'InCanvas':
                var r = this.$canvas.getBoundingClientRect();
                var x2 = r.width;
                var y2 = r.height;
                test = (p.mouseX > 0) && (p.mouseY > 0) && (p.mouseX < x2) && (p.mouseY < y2);
                break
            case "InButtons":
                var $btn = nodeOrElt;
                var r = this.$canvas.getBoundingClientRect();
                var rBtn = $btn.getBoundingClientRect();
                var x1 = rBtn.left - r.left;
                var y1 = rBtn.top - r.top;
                var x2 = x1 + rBtn.width;
                var y2 = y1 + rBtn.height;
                test = (p.mouseX > x1) && (p.mouseY > y1) && (p.mouseX < x2) && (p.mouseY < y2);
                break
            case 'InTooltip':
                var n = nodeOrElt;
                var h = this.$tooltip.clientHeight +12;
                var w = this.$tooltip.clientWidth/2 +6;
                var x1 = n.ctx.centerX - w;
                var x2 = n.ctx.centerX + w;
                var y1 = n.ctx.centerY - n.ctx.rayon - h;
                var y2;
                if (n === this.focusedNode) {
                    y2 = n.ctx.centerY - n.ctx.rayon*0.85;
                } else {
                    y2 = n.ctx.centerY - n.ctx.rayon*0.75;
                }
                test = (p.mouseX > x1) && (p.mouseX < x2) && (p.mouseY > y1) && (p.mouseY < y2);
                break
            default:
                console.error("Unknown condition: %s", cond)
        }
        //console.log(cond, "?", test);
        return test
    },

    //
    // Elm Ports
    //

    nodeClickedFromJs(node) {
        var nameid = node.data.nameid;
        this.app.ports.nodeClickedFromJs.send(nameid);
    },

    nodeHoveredFromJs(node) {
        var nid;
        if (!node) {
            nid = ""
        } else {
            nid = node.data.nameid;
        }
        if (this.app) this.app.ports.nodeHoveredFromJs.send(nid);
    },

    nodeFocusedFromJs(node) {
        // @DEBUG: why / where would node be undefined ?
        if (!node) return
        this.app.ports.nodeFocusedFromJs.send(this.getNodeData(node));
    },

    sendNodeDataFromJs(node) {
        this.app.ports.nodeDataFromJs.send(this.getNodeData(node));
    },

    //
    // Init
    //

    resizeMe() {
        if (new Date() - this.rtime < this.delta) {
            setTimeout(() => this.resizeMe(), this.delta);
        } else {
            this.timeout = false;

            this.$canvasButtons.classList.add("is-invisible");
            this.$tooltip.classList.add("is-invisible");

            this.computeGeometry();
            this.sizeDom();
            this.drawButtons();
            this.zoomToNode(this.focusedNode, 0.9);
        }
    },

    init_canvas() {
        this.$canvas = document.getElementById(this.canvasId);
        this.$canvasParent = document.getElementById(this.canvasParentId);
        this.$nextToChart = document.getElementById('nextToChart')
        if (!this.$canvas) return

        this.$canvas.classList.remove("is-invisible");
        this.ctx2d = this.$canvas.getContext("2d");

        this.$nextToChart.style.display = "flex";
        this.$nextToChart.style.flexDirection = "column";
        //this.$nextToChart.style.overflowY = "auto";

        this.sizeDom();
        this.clearCanvas(this.ctx2d);

        // Not ready
        //this.loading()
        setTimeout(() => this.drawStargate(0, 1), 10);
    },

    drawStargate(radius, down) {
        if (radius > 33) {
            down = -1
        } else if (radius <= 0) {
            down = 1
        }

        var ctx = this.ctx2d;
        var canvas = this.$canvas;

        // First
        var x = canvas.width / 2;
        var y = canvas.height / 2;
        var r = canvas.height/2.1 ;

        ctx.lineWidth = 5;
        ctx.strokeStyle = this.hoverCircleColor;
        ctx.shadowOffsetX = 0;
        ctx.shadowOffsetY = 0;
        ctx.shadowBlur = 3;
        ctx.shadowColor = '#656565';
        //ctx.fillStyle = this.colorCircle(0);
        ctx.fillStyle = shadeColor(this.colorCircle(0), -radius)+"55";

        ctx.beginPath();
        ctx.arc(x, y, r, 0, 2*Math.PI, false);
        //ctx.stroke();
        ctx.fill();

        //// Second
        //var x = canvas.width / 2;
        //var y = canvas.height / 2;
        //var r = canvas.height/2.1 - canvas.height/2.1/20 * radius  ;

        //ctx.lineWidth = 10;
        //ctx.strokeStyle = this.hoverCircleColor;
        //ctx.shadowOffsetX = 0;
        //ctx.shadowOffsetY = 0;
        //ctx.shadowBlur = 3;
        //ctx.shadowColor = '#656565';
        //ctx.fillStyle = this.colorCircle(1);

        //ctx.beginPath();
        //ctx.arc(x, y, r, 0, 2*Math.PI, false);
        //ctx.stroke();
        //ctx.fill();
        sleep(100).then(() => {
            if (this.isLoading) {
                this.clearCanvas(this.ctx2d);
                this.drawStargate(radius+down*4, down);
            }
        });
    },

    loading() {
        var r = this.$canvas.getBoundingClientRect();

        var canvas = this.$canvas;
		var ctx = this.ctx2d;
		var x = canvas.width / 2;
		var y = canvas.height / 2;
		var radius = canvas.height/2.1;

		ctx.lineWidth = 10;
		ctx.strokeStyle = this.hoverCircleColor;
		ctx.shadowOffsetX = 0;
		ctx.shadowOffsetY = 0;
		ctx.shadowBlur = 3;
		ctx.shadowColor = '#656565';

        var isLoading = this.isLoading;
		function animate(r) {
			ctx.beginPath();
			ctx.arc(x, y, r, 0, 2*Math.PI, false);
			ctx.stroke();
			r--;
            // @Debug, can't we stopt it !?
            if (r <= 0) {
                r = canvas.height/2.1;
            }
            if (isLoading) {
                requestAnimationFrame(function () {
                    animate(radius)
                });
            }
        }

        if (isLoading) {
            animate();
        }
    },

    // Init the canvas and draw the graph
    init(app, data, isInit) {
        var dataNodes = data.data;
        this.app = app;

        // Set the parent element
        this.$canvasParent = document.getElementById(this.canvasParentId);
        if (!this.$canvasParent) {
            console.warn("Canvas object not found, aborting")
            return
        }

        this.computeGeometry();

        //
        // Create and Bind Canvas to the DOM
        //

        // Create the visible canvas and context
        this.$canvas = document.getElementById(this.canvasId);
        this.$canvas.classList.remove("is-invisible");
        this.ctx2d = this.$canvas.getContext("2d");
        //this.ctx2d.clearRect(0, 0, this.width, this.height);
        setpixelated(this.ctx2d, true); // @debug: do we need this ?

        // Create a hidden canvas in which each circle will have a different color.
        // We use this to capture the clicked on circle
        var hiddenCanvas = d3.select("#"+this.canvasParentId).append("canvas")
            .attr("id", this.hiddenCanvasId)
            .attr("width", this.width)
            .attr("height", this.height)
            .style("display","none");
        this.$hiddenCanvas = hiddenCanvas.node();
        this.hiddenCtx2d = this.$hiddenCanvas.getContext("2d");
        //this.hiddenCtx2d.clearRect(0, 0,this.width, this.height);

        //
        // Update Html Elemens
        //

        // Resize height of parent sibling
        this.$nextToChart = document.getElementById('nextToChart')
        this.$nextToChart.style.display = "flex";
        this.$nextToChart.style.flexDirection = "column";
        //this.$nextToChart.style.overflowY = "auto";

        // Setup canvasButtons Buttons
        this.$canvasButtons = document.getElementById('canvasButtons');

        // Setup nodeTooltip Tooltip
        this.$tooltip = document.getElementById('nodeTooltip');
        this.clearNodeTooltip()

        this.sizeDom();

        //
        // Create Circle Packing - GraphPack
        //

        this.nodeSize = this.nodeSizeTopDown;
        this.resetGraphPack(dataNodes, true);

        /*////////////////////////////////////////////////////////////
        ////////////////// Events Handler callback ///////////////////
        ////////////////////////////////////////////////////////////*/

        // Listen for clicks on the main canvas
        var nodeClickEvent = e => {
            if (this.isZooming) {
                return false
            }
            if (this.isFrozen) {
                this.isFrozen = false;
                return true
            }
            var node = this.getNodeUnderPointer(e);
            var isUpdated = false;
            if (node) {
                isUpdated = true;
                if (node === this.focusedNode) {
                    // go to the parent node
                    if (node !== this.rootNode) {
                        node = node.parent;
                    } else {
                        isUpdated = false;
                    }
                }
            }

            if (isUpdated) {
                //this.clearNodeTooltip();
                this.clearNodeHover(node);
                this.nodeClickedFromJs(node);
            }

            return false;
        };

        // Listen for mouse moves/hooverin on the main canvas
        var canvasMouseMoveEvent = e => {
            if (this.isZooming) {
                return false
            }
            if (this.isFrozen) {
                return false
            }
            var p = this.getPointerCtx(e);
            var node = this.getNodeUnderPointer(e);        // @Warning, it updates ctx attributes.
            var isInTooltip = false;
            if (this.hoveredNode) {
                isInTooltip = this.checkIf(p, "InTooltip", this.hoveredNode);
                // Only show tooltip opion/ellipsis on hoover
                //if (isInTooltip) { this.nodeHoveredFromJs(this.hoveredNode); }
            }

            if (node) {
                if (node !== this.hoveredNode && !isInTooltip) {
                    this.drawNodeHover(node, true);
                }
            } else if (this.hoveredNode != this.focusedNode) {
                // @DEBUG: there is a little dead zone between circle.
                // When it happens, it goes there and focused node receive the hover...
                if (!isInTooltip) this.drawNodeHover(this.focusedNode, true);
            } else {
                // When it happens ?
                //this.drawNodeHover(this.focusedNode, true);
            }

            return false
        };

        // Listen for mouse entering canvas
        var canvasMouseEnterEvent = e => {
            if (this.isZooming) {
                return false
            }
            if (this.isFrozen) {
                return false
            }
            var p = this.getPointerCtx(e);
            var node = this.getNodeUnderPointer(e);        // @Warning, it updates ctx attributes.
            var isInTooltip = false;

            if (node != this.hoveredNode) { // avoid redrawing
                this.drawNodeHover(this.focusedNode, true);
            }

            return false
        }

        // Listen for mouse moves/hooverout on the main canvas
        var canvasMouseLeaveEvent = e => {
            var p = this.getPointerCtx(e);
            var isInCanvas = this.checkIf(p, "InCanvas", null); // purpose of that is possibliy linked to issue #9232dcd
            if (!isInCanvas) {
                // Remove the node hover and border
                var clearBorder = this.hoveredNode && (this.hoveredNode != this.focusedNode);
                if (clearBorder) {
                    this.clearNodeHover(this.hoveredNode);
                }
                // Set the hover by default on the focused node
                this.drawNodeHover(this.focusedNode, true);
            } else {
                if (this.isFrozen) {
                    return false
                }
                // Only show tooltip options/ellipsis on hoover
                //this.nodeHoveredFromJs(this.hoveredNode);
            }

            this.isFrozen = false;
            return false
        };


        // On Resize handle
        window.onresize = () => {
            this.rtime = new Date();
            if (this.timeout === false) {
                this.timeout = true;

                // Smooth redraw
                setTimeout( () => this.resizeMe(), this.delta);
            }
        };

        //////////////////////////////////////////////////////////////
        /////////////////////// Initiate /////////////////////////////
        //////////////////////////////////////////////////////////////

        console.log("Orga Canvas Initalization");
        this.isLoading = false;
		this.drawCanvas(true); // to add node.ctx

        //
        // Event listeners
        //

        // Canvas mouse event
        this.$canvas.addEventListener("mousemove", canvasMouseMoveEvent);
        this.$canvas.addEventListener("mouseenter", canvasMouseEnterEvent);
        this.$canvas.addEventListener("mouseleave", canvasMouseLeaveEvent);
        this.$canvas.addEventListener("click", nodeClickEvent);

		//this.$canvas.addEventListener("wheel", e => { // or window.addEventListener("scroll"....
        //    if (this.isZooming || this.isFrozen) { return e.preventDefault() }
        //    if (e.deltaY < 0){
        //        // upscroll code
        //        var node = this.getNodeUnderPointer(e);
        //        if (node && node != this.focusedNode) {
        //            e.preventDefault()
        //            nodeClickEvent(e)
        //        }
		//	} else if (e.deltaY > 0) {
		//		// downscroll code
        //        if (this.focusedNode && this.focusedNode.parent) {
        //            e.preventDefault()
        //            var node = this.focusedNode.parent;
        //            this.clearNodeHover(node);
        //            this.nodeClickedFromJs(node);
        //        }
		//	}
		//}, false);

        // Canvas button events redirection
        // Review -- Better implementation ?
        this.$canvasButtons.addEventListener("mousedown", e => {
            var p = this.getPointerCtx(e);
            var isInButtons = false;
            this.$canvasButtons.childNodes.forEach( o => {
                isInButtons |= this.checkIf(p, 'InButtons', o);
            });
            if (!isInButtons) {
                return nodeClickEvent(e)
            }
            return true
        });
        this.$canvasButtons.addEventListener("mousemove", e => {
            var p = this.getPointerCtx(e);
            var isInButtons = false;
            this.$canvasButtons.childNodes.forEach( o => {
                isInButtons |= this.checkIf(p, 'InButtons', o);
            });
            if (!isInButtons) {
                return canvasMouseMoveEvent(e)
            }
            return true
        });

        // Node Tooltip events
        var $subTooltip = document.getElementById(this.$tooltip.dataset.eventClick);
        $subTooltip.addEventListener("mousedown", e => {
            if (this.isFrozen) {
                this.isFrozen = false;
                return false
            }
            this.sendNodeDataFromJs(this.hoveredNode);
            this.isFrozen = false;
            return true
        });

        var $subTooltip = document.getElementById(this.$tooltip.dataset.eventHover);
        $subTooltip.addEventListener("mousedown", e => {
            this.isFrozen = true;
            return true
        });

        //
        // ELM Subscriptions
        //

        if (isInit) {

            // ToggleGrahReverse button
            app.ports.sendToggleGraphReverse.subscribe(e => {
                if (this.nodeSize.name == "nodeSizeTopDown") {
                    this.nodeSize = this.nodeSizeBottomUp;
                } else {
                    this.nodeSize = this.nodeSizeTopDown;
                }

                if (this.hoveredNode) this.clearNodeHover(this.hoveredNode);

                this.resetGraphPack(this.graph, false);
                this.clearCanvas(this.ctx2d);
                this.clearCanvas(this.hiddenCtx2d);
                this.zoomToNode(this.rootNode, 0.9);

            });

        }

        //
        // FPS Stats box
        //

        //var stats = new Stats();
        //stats.setMode(0); // 0: fps, 1: ms, 2: mb

        //// align top-left
        //stats.domElement.style.position = 'absolute';
        //stats.domElement.style.left = '0px';
        //stats.domElement.style.top = '0px';
        //document.body.appendChild( stats.domElement );
        return true

    },

};
