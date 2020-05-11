// @Debug use import with a bundler/webpack !

const setpixelated = (ctx2d, v) => {
    ctx2d['imageSmoothingEnabled'] = v;       /* standard */
    ctx2d['oImageSmoothingEnabled'] = v;      /* Opera */
    ctx2d['webkitImageSmoothingEnabled'] = v; /* Safari */
    ctx2d['msImageSmoothingEnabled'] = v;     /* IE */
    //ctx2d['mozImageSmoothingEnabled'] = v;    /* Firefox (deprecated) */
}

// Flat list of nodes (unordered) to nested tree structure
// from: https://stackoverflow.com/questions/18017869/build-tree-array-from-flat-array-in-javascript/40732240#40732240
const formatGraph = dataset =>  {
    var dataTree = [];
    var dataDict = Object.create(null);

    dataset.forEach( aData => dataDict[aData.nameid] = {
        ...aData,
        children : [],
        depth : 0
    });

    dataset.forEach( aData => {
        if(aData.parentid) {
            dataDict[aData.parentid].children.push(dataDict[aData.nameid])
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
        obj.children.forEach(d =>  {
            var d = computeDepth(d, currentdepth+1, obj.children.length-1);
            var tmpDepth = d.maxdepth;
            cumchild += d.cumchild;
            if (tmpDepth > maxdepth) {
                maxdepth = tmpDepth;
            }
        })
    }
    maxdepth = maxdepth + 1;
    cumchild = cumchild + 1;
    obj.cumchild = cumchild;
    return {maxdepth, cumchild};
}


const GraphPack = {

    // Background Colors
    backgroundColor: "#EDFCFF",

    // Graph Colors
    //var colorCircleRange: ['#d9d9d9','#838383','#4c4c4c','#1c1c1c', '#000000'],
    colorCircleRange: ['#bfbfbf','#838383','#4c4c4c','#1c1c1c', '#000000'],
    leafColor: "white",
    hoverCircleColor:  "black",
    focusCircleColor: "#368ED3", // "blue",
    hoverCircleWidth: 1.5, // waring, can break stroke with canvas drawing.

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
    circlesPadding: 1.8,

    // Graph fx settings
    minZoomDuration: 1250, // 1500
    zoomFactorCircle: 2.05,
    zoomFactorRole: 2.2,

    // Focus logics
    colToCircle : {}, // Dataset to swich between color of a circle (in the hidden canvas) and the node data
    nextCol: 1,
    colorCircle : null,
    rootNode    : null, // The root node of the graph
    focusedNode : null, // The node that has the active focus
    hoveredNode : null, // The node that is curently hoovered

    // Zooming
    ease: d3.easePolyInOut.exponent(3),
    //ease: d3.easePoly.exponent(4),
    isZooming: false,
    timeElapsed: 0,
    interpolator: null,
    vOld: null,

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

    // Data
    gPack: null, // Receive D3 data structure
    gStats: null, // Receive graph global statistics
    nodes: null,  // List of D3 nodes
    nodesDict: null, // Nodes mapping
    reason: null, // reason of init
    app: null, // elm app

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

    // Size the canvas
    computeGeometry() {

        this.computedWidth = this.$canvasParent.offsetWidth; //var computedWidth = parseInt(window.getComputedStyle($canvasParent).width, 10);
        this.computedHeight = (window.innerHeight)/2;

        // Canvas settings
        this.width = Math.max(this.computedWidth, this.minWidth);
        this.height = Math.max(this.computedHeight, this.minHeight); //(computedHeight > computedWidth ?  computedWidth: computedHeight );
        this.mobileSize = (window.innerWidth < 768 ? true : false);

        this.centerX = this.width/2;
        this.centerY = this.height/2;
        this.diameter = Math.min(this.width*0.97, this.height*0.97);
        this.zoomCtx = {
            // Init at CenterX, centerY
            centerX: this.centerX,
            centerY: this.centerY,
            scale: 1
        };
    },

    // Resize Html Elements created here
    sizeDom() {
        // Canvas
        this.$canvas.width = this.width;
        this.$canvas.height = this.height;
        this.$hiddenCanvas.width = this.width;
        this.$hiddenCanvas.height = this.height;

        // Element next to the canvas
        this.$nextToChart.style.minHeight = 2*this.height+"px";

        // Buttons
        var r = this.$canvas.getBoundingClientRect();
        //$canvasButtons.style.top = "-"+ height+"px"; // if position: relative
        this.$canvasButtons.style.left = r.left + r.width - this.$canvasButtons.offsetWidth -5 + "px";
        this.$canvasButtons.style.top = r.top + 10 + "px";

        this.$canvasButtons.classList.remove("is-invisible");
        this.$tooltip.classList.remove("is-invisible");
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
        var node,
            _name , type_ ,
            circleColor;
        // It's slightly faster than nodes.forEach()
        for (var i = 0; i < this.nodes.length; i++) {
            node = this.nodes[i];
            _name = node.data.name;
            type_ = node.data.type_;

            if (type_ === "Hidden") {
                continue;
            } else {
                this.addNodeCtx(node);
            }

            //If the hidden canvas was send into this function and it does not yet have a color,
            //generate a unique one.
            if(isHidden) {
                if(node.color === undefined) {
                    // If we have never drawn the node to the hidden canvas get a new color for it and put it in the dictionary.
                    node.color = this.genColor();
                    this.colToCircle[node.color] = node;
                }
                // On the hidden canvas each rectangle gets a unique color.
                circleColor = node.color;
            } else {
                circleColor = (type_ === "Circle") ? this.colorCircle(node.depth) : this.leafColor;
            }

            //Draw each circle
            ctx2d.beginPath();
            ctx2d.fillStyle = circleColor;
            ctx2d.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon,
                0, 2 * Math.PI, true);
            ctx2d.fill();

            if (!isHidden) {
                if (node.isHovered) {
                    ctx2d.lineWidth = this.hoverCircleWidth;
                    ctx2d.strokeStyle = this.focusCircleColor;
                    ctx2d.stroke();
                }

                if (type_ === "Role") {
                    var text = _name.substring(0,3).replace(/./,x=>x.toUpperCase())
                    var font_size = 19;
                    var text_display = false;
                    //for (var ii=0; ii < 2; ii++) {
                    // Search font that fit
                    ctx2d.font = font_size +"px Arial";
                    if (ctx2d.measureText(text).width+1 < node.ctx.rayon*2) {
                        text_display = true;
                        //break;
                    } else {
                        font_size--;
                    }
                    //}

                    if (text_display) {
                        ctx2d.beginPath();
                        ctx2d.fillStyle = "black";
                        ctx2d.textAlign = "center";
                        ctx2d.fillText(text,
                            node.ctx.centerX, node.ctx.centerY+7);
                        //ctx2d.shadowColor = '#999';
                        //ctx2d.shadowBlur = 20;
                        //ctx2d.shadowOffsetX = 5;
                        //ctx2d.shadowOffsetY = 5;
                        ctx2d.fill();
                    }
                } else {
                    //if (focusedNode.depth == node.depth || focusedNode.depth == node.depth-1 ) {
                    //    ctx2d.beginPath();
                    //    ctx2d.fillStyle = "white";
                    //    ctx2d.fillCircleText(_name,
                    //        node.ctx.centerX, node.ctx.centerY,
                    //        node.ctx.rayon, -Math.PI*0.7);
                    //    ctx2d.fill()
                    //}
                }
            }
        }//for i
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
                console.warn("Unknown node:", focus );
                return
            }
            focus = maybeFocus;
        } else {
            // assume node
            // pass
        }

        var elmHasBeenUpdated = false;
        if (this.focusedNode.ctx) {
            this.clearNodeHover(this.focusedNode);
            this.nodeFocusedFromJs(focus);
            elmHasBeenUpdated = true;
        }
        this.focusedNode = focus;

        var zoomFactor = this.zoomFactorCircle;
        if (this.focusedNode.data.type_ === 'Role') {
            zoomFactor = this.zoomFactorRole;
        }
        var v = [this.focusedNode.x, this.focusedNode.y, this.focusedNode.r * zoomFactor]; //The center and width of the new "viewport"
        var maxDuration = this.minZoomDuration*2;
        delay = (delay === undefined ? 0 : delay*this.minZoomDuration);

        interpolator = d3.interpolateZoom(this.vOld, v); //Create interpolation between current and new "viewport"
        duration = Math.max(interpolator.duration, maxDuration/2); //Interpolation gives back a suggested duration
        timeElapsed = 0+delay; //Set the time elapsed for the interpolateZoom function to 0
        this.vOld = v; //Save the "viewport" of the next state as the next "old" state

        //Perform the interpolation and continuously change the zoomCtx while the "transition" occurs.
        var interpolateZoom = (dt) => {
            if (interpolator) {
                timeElapsed += dt;
                var t = this.ease(timeElapsed / duration);

                this.zoomCtx.centerX = interpolator(t)[0];
                this.zoomCtx.centerY = interpolator(t)[1];
                this.zoomCtx.scale = this.diameter / interpolator(t)[2];

                if (timeElapsed >= duration)
                {
                    interpolator = null;
                    return true;
                } else {
                    return false;
                }
            }
        };

        var	dt = 0;
        var t = d3.timer((elapsed) => {
            //stats.begin();
            this.isZooming = true;
            var finished = interpolateZoom(elapsed - dt);
            dt = elapsed;
            this.drawCanvas();
            //stats.end();
            if (finished || elapsed > maxDuration) {
                this.isZooming = false;
                //this.drawCanvas();
                this.drawCanvas(true);
                if (!elmHasBeenUpdated) this.nodeFocusedFromJs(this.focusedNode); // INIT
                this.drawNodeHover(this.focusedNode);
                t.stop();
            }
        });

    },


    //
    // D3/GraphPack
    //


    // Determine the node size in the circle packing
    // Returns: int f(n.depth, n.neigbor, n.cumchild)
    nodeSizeTopDown(n, stats) {
        var size = 10000/(stats.maxdepth)**(Math.max(1.5, n.depth))
        return size
    },

    nodeSizeBottomUp(n, stats) {
        var sizeDefault = 4;
        var size = 10000/(stats.maxdepth)**(Math.max(0, sizeDefault - n.depth))
        return size
    },

    // Init and create the GraphPack data structure
    resetGraphPack(graph) {
        // Mapping function from a node depth to color.
        this.colorCircle = d3.scaleOrdinal()
            .domain(Array.from({length:this.colorCircleRange.length},(v,k)=>k))
            .range(this.colorCircleRange)
            .unknown(this.backgroundColor); // window.getComputedStyle(document.getElementById("body"), null).getPropertyValue("background-color");

        // Determine the node order in the circle packing
        const nodeOrder = (n1, n2) => {
            // n1.createdAt < n2.createdAt // node order
            return 0
        }

        this.gStats = computeDepth(graph);
        this.gPack = d3.pack()
            .padding(this.circlesPadding)
            .size([this.diameter, this.diameter])
        (d3.hierarchy(graph)
            .sum(d => this.nodeSize(d, this.gStats))
            .sort(nodeOrder));

        this.nodesDict = Object.create(null);
        this.nodes = this.gPack.descendants(graph);
        this.rootNode = this.nodes[0];
        this.focusedNode = this.rootNode;
        this.hoveredNode = null;
        this.nodes.forEach( n => this.nodesDict[n.data.nameid] = n);
        this.vOld = [this.focusedNode.x, this.focusedNode.y, this.focusedNode.r * this.zoomFactorCircle];

        this.graph = graph;
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

    // Returns the path from root to node.
    getNodePath(node) {
        var path = this.gPack.path(node).map(n => {
            return {
                nidjs: n.color,
                nameid: n.data.nameid,
                name: n.data.name,
            };
        });
        return path
    },

    // Get node position and properties
    addNodeCtx(node) {
        var zoomCtx = this.zoomCtx;
        var ctx, centerX, centerY, rayon;

        centerX = ((node.x - zoomCtx.centerX) * zoomCtx.scale) + this.centerX;
        centerY = ((node.y - zoomCtx.centerY) * zoomCtx.scale) + this.centerY;
        if (node.data.type_ === "Role") {
            rayon = node.r * 0.95 ;
        } else {
            rayon = node.r;
        }
        rayon *= zoomCtx.scale;
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
        var col = hiddenCtx2d.getImageData(p.mouseX, p.mouseY, 1, 1).data;
        //Our map uses these rgb strings as keys to nodes.
        var colString = "rgb(" + col[0] + "," + col[1] + ","+ col[2] + ")";
        var node = this.colToCircle[colString];
        if (node) {
            this.addNodeCtx(node);
        }
        return node;
    },

    // Draw node border
    // If changeFocus is true, the focused node borer is removed
    drawNodeHover(node) {
        var ctx2d = this.ctx2d;
        if (!node.ctx) {
            console.warn("node.ctx us undefined here; Add a timeout on init event listeners...");
            return false
        }
        clearBorder = this.hoveredNode && (this.hoveredNode != this.focusedNode);
        if (clearBorder) this.clearNodeHover(this.hoveredNode);

        var circleColor = this.hoverCircleColor;
        if (node == this.focusedNode) circleColor = this.focusCircleColor;

        // Draw border
        ctx2d.beginPath();
        ctx2d.arc(node.ctx.centerX, node.ctx.centerY,
            node.ctx.rayon+1, 0, 2 * Math.PI, true);
        ctx2d.lineWidth = this.hoverCircleWidth;
        ctx2d.strokeStyle = circleColor;
        ctx2d.stroke();

        // Draw tooltip
        this.drawNodeTooltip(node);

        // Update global context
        node.isHovered = true;
        this.hoveredNode = node; //@debug: use globCtx
        return
    },

    // Clean node hovering
    clearNodeHover(node) {
        var ctx2d = this.ctx2d;

        // Clear node Border
        ctx2d.beginPath();
        ctx2d.arc(node.ctx.centerX, node.ctx.centerY,
            node.ctx.rayon+1, 0, 2 * Math.PI, true);
        ctx2d.lineWidth = 3;
        ctx2d.strokeStyle = this.colorCircle(node.depth-1);
        ctx2d.stroke();

        // Clear node tooltip
        this.clearNodeTooltip();

        // Update context
        node.isHovered = false;
        this.hoveredNode = null; //@debug: use globCtx
        return
    },

    // Draw the node tooltip
    drawNodeTooltip(node) {
        var $tooltip = this.$tooltip
        var r = this.$canvas.getBoundingClientRect();
        // == add tooltip
        // @DEBUG: tooltip neeed to be displayed to get its clientWidth.
        //$tooltip.textContent = node.data.name;
        $tooltip.childNodes[0].textContent = node.data.name;
        $tooltip.dataset.nid = node.data.id;
        $tooltip.classList.remove("fadeOut");
        $tooltip.classList.add("fadeIn");
        // --
        var bodyRect = document.querySelector("body").getBoundingClientRect();
        var scrollLeft = bodyRect.left;
        var scrollTop = bodyRect.top;
        var tw = ($tooltip.clientWidth);
        var hw = ($tooltip.clientHeight + 2*node.ctx.rayon);
        var l = (node.ctx.centerX + r.left - scrollLeft  - (tw/2 + 1));
        var t = (node.ctx.centerY + r.top - scrollTop  - (hw/2 + 23));
        if (l+tw-r.left < 0 || t+hw-r.top < 0 || r.left+r.width-tw-l < 0 ) {
            // the tooltip overflow "too moch" outside the canvas.
            this.clearNodeTooltip();
        } else {
            $tooltip.style.left = l + "px";
            $tooltip.style.top = t + "px";
        }
        return
    },

    // Clear node tooltip.
    clearNodeTooltip() {
        this.$tooltip.classList.remove("fadeIn");
        this.$tooltip.classList.add("fadeOut");
        //this.$tooltip.style.display = "none";
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
                var w = this.$tooltip.clientWidth/2 +8;
                var x1 = n.ctx.centerX - w;
                var x2 = n.ctx.centerX + w;
                var y1 = n.ctx.centerY - n.ctx.rayon - h;
                var y2;
                if ( n === this.focusedNode) {
                    y2 = n.ctx.centerY - n.ctx.rayon*0.85;
                } else {
                    y2 = n.ctx.centerY - n.ctx.rayon*0.6;
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
        var nodeFocus = {
            rootid: this.rootNode.data.nameid,
            nameid: node.data.nameid,
            isRoot: node === this.rootNode,
        };
        this.app.ports.nodeClickedFromJs.send(nodeFocus);
    },

    nodeFocusedFromJs(node) {
        // @DEBUG: why / where would node be undefined ?
        if (!node || this.reason == "resize") {
            this.reason = "";
            return
        }
        var nodePath = this.getNodePath(node);
        this.app.ports.nodeFocusedFromJs.send(nodePath);
    },

    sendNodeDataFromJs(node) {
        this.app.ports.rawNodeDataFromJs.send(node.data);
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
            this.zoomToNode(this.focusedNode, 0.9);
        }
    },

    // Init the canvas and draw the graph
    init(app, data, reason) {
        var dataNodes = data.data;
        if (dataNodes.length == 0) {
            console.warn("Graph is empty, aborting")
            return
        }

        this.app = app;
        this.reason = reason;

        // Set the parent element
        this.$canvasParent = document.getElementById(this.canvasParentId);
        this.computeGeometry();

        //
        // Create and Bind Canvas to the DOM
        //

        // Create the visible canvas and context
        var canvas = d3.select("#"+this.canvasParentId).append("canvas")
            .attr("id", this.canvasId)
            .attr("width", this.width)
            .attr("height", this.height);
        this.$canvas = canvas.node();
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
        // If buttons not outside the canvas, cause error !
        //setTimeout(() => {
        //    this.sizeDom();
        //}, 200);

        //
        // Create Circle Packing - GraphPack
        //

        var graph = formatGraph(dataNodes);
        if (graph.length > 1) console.warn("More than 1 graph given -> Some nodes are not connected.")

        this.nodeSize = this.nodeSizeTopDown;
        this.resetGraphPack(graph[0]);

        /*////////////////////////////////////////////////////////////
        ////////////////// Events Handler callback ///////////////////
        ////////////////////////////////////////////////////////////*/

        // Listen for clicks on the main canvas
        var nodeClickEvent = e => {
            if (this.isZooming) {
                return false
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
                this.clearNodeTooltip();
                //this.zoomToNode(node); @DEBUG: change behaviour, zoom from elm init
                this.nodeClickedFromJs(node);
            }

            return false;
        };

        // Listen for mouse moves/hooverin on the main canvas
        var canvasMouseMoveEvent = e => {
            if (this.isZooming) {
                return false
            }
            var p = this.getPointerCtx(e);
            var node = this.getNodeUnderPointer(e);        // @Warning, it updates ctx attributes.
            var isInTooltip = false;
            if (this.hoveredNode) {
                isInTooltip = this.checkIf(p, "InTooltip", this.hoveredNode);
            }

            if (node) {
                if (node !== this.hoveredNode && !isInTooltip) {
                    this.drawNodeHover(node);
                }
            } else if (this.hoveredNode) {
                //var isInCanvas = this.checkIf(p, "InCanvas", null); // possibliy link to issue #9232dcd
                //if (!isInTooltip && isInCanvas) this.clearNodeHover(hoveredNode);
                // pass...
            } else {
                this.drawNodeHover(this.focusedNode);
            }
            return false
        };

        // Listen for mouse moves/hooverout on the main canvas
        var canvasMouseLeaveEvent = e => {
            var p = this.getPointerCtx(e);
            var isInCanvas = this.checkIf(p, "InCanvas", null); // purpose of that is possibliy linked to issue #9232dcd
            if (!isInCanvas) this.drawNodeHover(this.focusedNode);

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
        this.drawCanvas(true); // to add node.ctx

        //
        // Event listeners
        //

        // Canvas mouse event
        this.$canvas.addEventListener("mousemove", canvasMouseMoveEvent);
        this.$canvas.addEventListener("mouseleave", canvasMouseLeaveEvent);
        this.$canvas.addEventListener("mousedown", nodeClickEvent);
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
        this.$tooltip.addEventListener("mousedown", e => {
            this.sendNodeDataFromJs(this.hoveredNode);
            document.documentElement.classList.add('has-modal-active');
            return true
        });

        //
        // ELM Subscriptions
        //

        app.ports.sendToggleGraphReverse.subscribe(e => {
            if (this.nodeSize.name == "nodeSizeTopDown") {
                this.nodeSize = this.nodeSizeBottomUp;
            } else {
                this.nodeSize = this.nodeSizeTopDown;
            }

            if (this.hoveredNode) this.clearNodeHover(this.hoveredNode);

            this.resetGraphPack(this.graph);

            this.clearCanvas(this.ctx2d);
            this.clearCanvas(this.hiddenCtx2d);
            this.zoomToNode(this.rootNode, 0.9);

        });

        app.ports.sendToggleTooltips.subscribe(e => {
            //DEBUG: TODO
        });

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

    },

};