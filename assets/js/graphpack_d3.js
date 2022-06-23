import { select, selectAll } from 'd3-selection'
import { scaleOrdinal } from 'd3-scale'
import { timer } from 'd3-timer'
import { interpolateZoom } from 'd3-interpolate'
import { easePolyOut, easePolyInOut } from 'd3-ease'
import { hierarchy, pack } from 'd3-hierarchy'
import { shadeColor, setpixelated, sleep, ptInTriangle } from './custom.js'

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

const NodeVisibility = {
    Public: "Public",
    Private: "Private",
    Secret: "Secret",
}

// Flat list of nodes (unordered) to nested tree structure
// from: https://stackoverflow.com/questions/18017869/build-tree-array-from-flat-array-in-javascript/40732240#40732240
const formatGraph = dataset =>  {
    var dataTree = [];
    var dataDict = Object.create(null);

    dataset.forEach((aData, i) => {
        dataDict[aData.nameid] = {
            ...aData,
            children : [],
            depth : 0
        }
    });

    dataset.forEach( aData => {
        // Filter Speciale Role nodes
        if (aData.role_type == RoleType.Member || aData.role_type == RoleType.Owner) {
            delete dataDict[aData.nameid]
            return
        }

        // Filter hidden node
        //if (!aData.parent && aData.nameid.split("#").length > 1) {
        //    delete dataDict[aData.nameid]
        //    return
        //}

        if(aData.parent) {
            // If private date contains public, some parent may be
            // hidden here.
            if (!dataDict[aData.parent.nameid]) {
                aData.parent = null;
                dataTree.push(dataDict[aData.nameid])
            } else {
                dataDict[aData.parent.nameid].children.push(dataDict[aData.nameid])
            }
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
    return {maxdepth, cumchild}
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
    roleColors: {
        [RoleType.Coordinator]: "#ffdaa1",
        [RoleType.Guest]: "#f4fdf5",
        [RoleType.Bot]: "#eeeeee",
        //[RoleType.Bot]: "#81b987",
        //[RoleType.Bot]: "rgb(0,0,0,0)", // transparent
        "_default_": "#a2b9df" // "#edf5ff"// "#f0fff0", // "#FFFFF9"
    },
    usernameColor: "#8282cc",
    nameColor: "#172335",
    focusCircleColor: "#4a79ac", // blue>"#368ed3"
    focusCircleWidth: 6, // warning, can break stroke with canvas drawing.
    hoverCircleColor: "#555", //  grey-black>"#3f3f3f"
    hoverCircleWidth: 2,
    outsideZoomOpacity: "75",

    // Html element ID
    canvasParentId: "canvasParent",
    canvasId: "canvasOrga",
    hiddenCanvasId: "hiddenCanvasOrga",

    // Geometry
    minWidth: 300,
    minHeight: 444,
    width: null,
    height: null,
    mobileSize: null,
    // Nodes/Circles geometry
    centerX: null,
    centerY: null,
    rayon: null,
    zoomCtx: null,
    circlesPadding: 8, // 1.8
    fontsizeCircle_start: 22,
    fontsizeRole_start: 19,
    fontstyleCircle: "Arial, fractaleicon",

    // Graph fx settings
    isLoading: true,
    minZoomDuration: 450, // 1250
    zoomFactorRoot: 2.02,
    zoomFactorCircle: 2.02,
    zoomFactorRole: 6,
    zoomFactorGuest: 6,
    // rayon size of the node in the canvas
    rayonFactorRole: 0.95,
    rayonFactorGuest: 0.75,
    rayonFactorBot: 0.5,
    guestSizeDivider: 1,
    // y-axis offset for the top node
    nodeOffsetY: 0,

    // State
    colToCircle : {}, // Dataset to swich between color of a circle (in the hidden canvas) and the node data
    nextCol     : 1,
    colorCircle : null,
    rootNode    : null, // The root node of the graph
    focusedNode : null, // The node that has the active focus
    zoomedNode  : null, // The node that has is centered
    hoveredNode : null, // The node that is curently hoovered
    isFrozen    : false, // Tooltip click state
    isFrozenMenu: false, // Tooltip right click state
    handlers: [],

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
    $welcomeButtons: null,
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

        // Draw canvas buttons
        this.$canvasButtons.style.left = r.left + r.width - this.$canvasButtons.offsetWidth -8 -scrollLeft +"px";
        this.$canvasButtons.style.top = r.top + 13 -scrollTop +"px";
        this.$canvasButtons.classList.remove("is-invisible");

        // Draw welcome buttons
        if (this.$welcomeButtons) {
            this.$welcomeButtons.style.left = r.left + r.width/2 - this.$welcomeButtons.offsetWidth/2 +8 -scrollLeft +"px";
            this.$welcomeButtons.style.top = r.top + 100 -scrollTop +"px";
            this.$welcomeButtons.classList.remove("is-invisible");
        }

        // Tooltip
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

        this.rayon = (Math.min(this.width*0.97, this.height*0.97)) / 2;
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
        if (!this.graph) return

        var ctx2d;
        if (isHidden) {
            ctx2d = this.hiddenCtx2d;
        } else {
            ctx2d = this.ctx2d;
        }
        this.clearCanvas(ctx2d);

        //Select our dummy nodes and draw the data to canvas.
        this.drawCurrent(isHidden, ctx2d)

    },

    drawCurrent(isHidden, ctx) {
        // Separator between the opaque nodes and the other.
        var boundary = this.focusedNode;

        // First draw the node above the zoomedNode and their children (opacity)
        this.drawOutside(boundary, isHidden, ctx);

        // Then draw the zoomedNode/focusedNode and descendends
        this.drawInside(boundary, isHidden, ctx);

        if (!isHidden)
            // Draw names when zooming in/out
            this.drawNodeNames(this.zoomedNode)

    },

    // Not ready...Doesn't work.
    drawCanvasLight() {
        this.clearCanvas(this.ctx2d);

        // Separator between the opaque nodes and the other.
        var boundary = this.focusedNode;

        // First draw the node above the zoomedNode and their children (opacity)
        this.drawOutside(boundary, false, this.ctx2d, 100);

        // Then draw the zoomedNode/focusedNode and descendends
        this.drawInside(boundary, false, this.ctx2d, 100);
    },


    drawOutside(b, isHidden, ctx, max_draw) {
        // list of nodes to draw.
        var tree;
        if (b.parent) {
            // No more than X ancestors (more will cause deep node to be to dark...)
            tree = b.parent.ancestors().slice(0, 2).reverse();
            if (b.parent.children) {
                if (this.zoomedNode == this.focusedNode) {
                    tree.push(...b.parent.children.filter(x => x.data.nameid !== b.data.nameid));
                } else {
                    tree.push(...b.parent.children);
                }
            } else {
                tree.push(b.parent);
            }
            // It's slightly faster than .forEach()
            for (var i = 0; i < tree.length; i++) {
                this.drawNode(tree[i], isHidden, ctx, this.outsideZoomOpacity);
                if (i > max_draw) break
            }
        }
    },

    drawInside(b, isHidden, ctx, max_draw) {
        // list of nodes to draw.
        var tree = b.descendants();
        var cpt = 0;
        for (var i = 0; i < tree.length; i++) {
            let d = tree[i].depth - this.focusedNode.depth
            if (d >= 0 && d < 4 || tree[i].depth == 0) {
                this.drawNode(tree[i], isHidden, ctx);
                if (i > max_draw) break
                cpt++;
            }
        }

        if (!isHidden && !max_draw) {
            // Draw focused border
            var w = this.focusCircleWidth;
            var color = this.focusCircleColor;
            // Draw border
            ctx.beginPath();
            ctx.arc(b.ctx.centerX, b.ctx.centerY, b.ctx.rayon+0.1+ w*0.5,
                0, 2 * Math.PI, true);
            ctx.lineWidth = w;
            ctx.strokeStyle = color;
            ctx.stroke();
        }

    },

    drawNode(node, isHidden, ctx, opac) {
        var circleColor;

        if (node.data.type_ === "Hidden") return
        else this.addNodeCtx(node);

        // Get the circle Color
        if(isHidden) {
            // On the hidden canvas each rectangle gets a unique color.
            circleColor = node.colorid;
        } else if (opac && (opac[0] == "#" || !opac.length)) {
            // Given color OR given colorGradient (ot length)
            circleColor = opac;
        } else {
            circleColor = this.getNodeColor(node, opac);
        }

        // Draw node
        switch (node.data.type_) {
            case NodeType.Role:
                //var r = node.ctx.rayon*0.75;
                //var l = node.ctx.rayon/20;
                //ctx.beginPath();
                //ctx.fillStyle = circleColor;
                //ctx.ellipse(node.ctx.centerX, node.ctx.centerY, r+l, r-l, 0, 0, Math.PI * 2, false);
                //ctx.ellipse(node.ctx.centerX, node.ctx.centerY, r-l, r+l, 0, 0, Math.PI * 2, false);
                //ctx.fill();
                ctx.beginPath();
                ctx.fillStyle = circleColor;
                ctx.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon, 0, 2 * Math.PI, true);
                ctx.fill();
                break;
            default:
                ctx.beginPath();
                ctx.fillStyle = circleColor;
                ctx.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon, 0, 2 * Math.PI, true);
                ctx.fill();
        }

        // Draw owned Role
        if ((!isHidden && this.uctx && node.data.first_link) && this.uctx.username == node.data.first_link.username) {
            // Draw user pin
            //var r =  Math.max(10 - (node.depth - this.focusedNode.depth) , 1)/4
            //ctx.beginPath();
            //ctx.fillStyle = "green";
            //ctx.arc(node.ctx.centerX, node.ctx.centerY + node.ctx.rayon*3/4 , r,
            //    0, 2 * Math.PI, true);
            //ctx.fill();

            // Draw user dashed border
            var w = 2;
            var color = "green";
            ctx.beginPath();
            ctx.setLineDash([5, 5]);
            ctx.beginPath();
            ctx.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon- w*0.5,
                0, 2 * Math.PI, true);
            ctx.lineWidth   = w;
            ctx.strokeStyle = color;
            ctx.stroke();
            ctx.setLineDash([]);
        }
    },

    drawNodeNames(node) {
        var n, opac;
        var defOpac = (node == this.focusedNode)

        for (var i=0; i < node.data.children.length; i++) {
            n = node.children[i];
            if (!n.ctx || node.depth !== n.depth-1) continue

            // Draw names
            if (defOpac) {
                opac = "ff";
            } else if (n == this.focusedNode) {
                opac = "ad";
            } else {
                opac = "80";
            }
            if (n.data.type_ === NodeType.Circle && this.circles_len < 100 ) {
                this.drawCircleName(n, opac)
            } else if (this.roles_len < 100) {
                this.drawRoleName(n, opac)
            }
        }

    },

    drawCircleName(node, opac) {
        // Draw circle names.
        var ctx2d = this.ctx2d
        var fontSize = this.fontsizeCircle_start;
        var text, textWidth;

        // Name
        text = node.data.name;
        textWidth = ctx2d.measureText(text).width;
        if (textWidth > node.ctx.rayon*2.5) {
            text = text.split(" ").map(s => s.substring(0, 3)).join("·")
            textWidth = ctx2d.measureText(text).width;
            if (textWidth > node.ctx.rayon*2.5) {
                text = text.split("·").map(s => s.substring(0, 1)).join("·")
            }
        }

        ctx2d.beginPath();
        ctx2d.font = "bold " + fontSize + "px " + this.fontstyleCircle;
        ctx2d.textAlign = "center";
        if (node.depth <= 1)
            ctx2d.lineWidth = 1;
        else
            ctx2d.lineWidth = 2;
        ctx2d.strokeStyle = "#5e6d6f" + opac;
        ctx2d.fillStyle = this.nameColor + opac;
        if (node.data.visibility !== this.getParent(node).data.visibility) {
            if (node.data.visibility == NodeVisibility.Public) {
                text = "\ue960 " + text;
            } else if (node.data.visibility == NodeVisibility.Private) {
                text = "\ue930 " + text;
            } else if (node.data.visibility == NodeVisibility.Secret) {
                text = "\ue91a " + text;
            }
        }
        ctx2d.strokeText(text, node.ctx.centerX, node.ctx.centerY - node.ctx.rayon*0.4);
        ctx2d.fillText(text, node.ctx.centerX, node.ctx.centerY - node.ctx.rayon*0.4);
        ctx2d.fill();
        ctx2d.stroke();

        // Set some text around the circle
        //ctx2d.beginPath();
        //ctx2d.font = "12px " + this.fontstyleCircle;
        //var h = ctx2d.measureText('M').width;
        ////ctx2d.shadowColor = "#999"; //ctx2d.shadowBlur = 10; //ctx2d.shadowOffsetX = 1; //ctx2d.shadowOffsetY = 1;
        //ctx2d.fillStyle = "dark";
        //ctx2d.fillText("\ue960", node.ctx.centerX, node.ctx.centerY + node.ctx.rayon - h/3);
        //ctx2d.fill()
    },

    drawRoleName(node, opac) {
        // Draw Role name and username
        var ctx2d = this.ctx2d
        var fontSize = this.fontsizeRole_start;
        var text, textWidth, textHeight = ctx2d.measureText('M').width;

        // Name
        if (node.data.role_type == RoleType.Guest) return

        text = node.data.name;
        var textWidth = ctx2d.measureText(text).width;
        if (textWidth+textHeight/2 > node.ctx.rayon*2) {
            text = text.split(" ").map(s => s.substring(0, 3)).join("·")
            textWidth = ctx2d.measureText(text).width;
            if (textWidth+textHeight/2 > node.ctx.rayon*2) {
                text = text.split("·").map(s => s.substring(0, 1)).join("·")
            }
        }

        // final
        if (text) {
            ctx2d.beginPath();
            ctx2d.font = fontSize + "px " + this.fontstyleCircle;
            ctx2d.textAlign = "center";
            // Color
            if (node.data.color) {
                ctx2d.fillStyle = this.colorToTextColor(node.data.color);
            } else {
                ctx2d.fillStyle = this.nameColor + opac;
            }
            ctx2d.fillText(text, node.ctx.centerX, node.ctx.centerY);
            ctx2d.fill();
            // tips
            ctx2d.fillStyle = this.nameColor + "99";
            if (node.data.role_type == RoleType.Bot) {
                //ctx2d.fillText('🤖', node.ctx.centerX, node.ctx.centerY-node.ctx.rayon*0.5);
                ctx2d.fillText('\ue962', node.ctx.centerX, node.ctx.centerY-node.ctx.rayon*0.45);
            } else if (node.data.type_ == NodeType.Role) {
                ctx2d.fillText('\uf06c', node.ctx.centerX, node.ctx.centerY-node.ctx.rayon*0.45);
            }
            ctx2d.fill();

            // Username
            if (node.data.first_link) {
                var text_username = null;
                ctx2d.font = fontSize-7 + "px " + this.fontstyleCircle;
                text_username = "@"+node.data.first_link.username;
                textWidth = ctx2d.measureText(text_username).width;
                if (textWidth > node.ctx.rayon*2)
                    text_username = "@"

                ctx2d.beginPath();
                ctx2d.fillStyle = this.usernameColor;
                ctx2d.fillText(text_username, node.ctx.centerX, node.ctx.centerY + node.ctx.rayon*0.4);
                ctx2d.fill();
            }
        }
    },

    // Draw node border + eventually tooltip
    drawNodeHover(node, doDrawTooltip) {
        if (!node.ctx) {
            // Wait for the canvas to render before drawing border.
            // If not, focus border won be draw if another circle in hover before rendering.
            return false
        }

        // Clear Border
        var clearBorder = this.hoveredNode && (this.hoveredNode != this.focusedNode);
        if (clearBorder) this.clearNodeHover();

        // Draw Border (on hoover)
        if (node != this.hoveredNode && node != this.focusedNode) {
            var ctx2d = this.ctx2d;

            var color, w;
            if (node == this.focusedNode) {
                color = this.focusCircleColor;
                w = this.focusCircleWidth;
            } else {
                color = this.hoverCircleColor;
                w = this.hoverCircleWidth;
            }

            // Draw Circle border
            ctx2d.beginPath();
            ctx2d.lineWidth = w;
            ctx2d.strokeStyle = color;
            ctx2d.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon+0.1+ w/2, 0, 2 * Math.PI, true);
            ctx2d.stroke();
            //ctx2d.save();


        }

        // Draw tooltip
        if (doDrawTooltip) {
            this.drawNodeTooltip(node);
        }

        // Update global context
        this.hoveredNode = node; //@debug: use globCtx
        return
    },

    // Clean node hovering
    clearNodeHover() {
        if (!this.hoveredNode) return

        // Remove the circle border
        var node = this.hoveredNode;
        var w;
        if (node == this.focusedNode) w = this.focusCircleWidth;
        else w = this.hoverCircleWidth;
        //this.ctx2d.restore();
        this.ctx2d.lineWidth = w*1.5;
        this.ctx2d.strokeStyle = this.getNodeColor(node.parent || this.rootNode);
        this.ctx2d.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon+0.1 + w/2, 0, 2 * Math.PI, true);
        this.ctx2d.stroke();

        // Fix canvas alteration (text cutted and opacity stacked)
        if (this.focusedNode == this.zoomedNode)
            this.drawInside(this.focusedNode, false, this.ctx2d);
        else {
            // Redraw zommed node with the color of its parent.
            //1) This allow opacity color to be consitent !
            var p = this.zoomedNode.parent;
            // First reset the node colors
            this.drawNode(this.zoomedNode, false, this.ctx2d, this.backgroundColor);
            //2) Then redraw the inner circle with its parents colors.
            for (var j=0; j < this.zoomedNode.depth; j++) {
                var color = this.getNodeColor(p, this.outsideZoomOpacity);
                this.drawNode(this.zoomedNode, false, this.ctx2d, color);
                parent = p.parent;
            }

            // Redraw the zommed node before redrawing names
            var tree = [this.zoomedNode, ...this.zoomedNode.children]
            for (var i = 0; i < tree.length; i++) {
                this.drawNode(tree[i], false, this.ctx2d, this.outsideZoomOpacity);
            }

            this.drawInside(this.focusedNode, false, this.ctx2d);
        }
        this.drawNodeNames(this.zoomedNode);

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
        }, 25);
    },
    drawNodeTooltip_(node) {
        var $tooltip = this.$tooltip
        // == add tooltip
        // @warning: tooltip neeed to be displayed to get its clientWidth.
        var $subTooltip = document.getElementById(this.$tooltip.dataset.eventTension);
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
        if (node == this.focusedNode == this.zoomedNode) {
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


    // Create the interpolation function between current view and the clicked on node.
    // It firsts zoom to get the circles to the right location
    // then timer the interpolateZoom and rendering.
    // If `delay` is given, it overwrite the zoom duration. Give a low value for flush reset.
    zoomToNode(focus, delay) {
        //Based on the generous help by Stephan Smola
        //http://bl.ocks.org/smoli/d7e4f9199c15d71258b5
        if (this.isZooming) return false

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
            this.clearNodeHover();
            this.nodeFocusedFromJs(focus);
            elmHasBeenUpdated = true;
        }
        var oldFocus = this.focusedNode;
        this.focusedNode = focus;
        this.drawNodeHover(this.focusedNode, false); // why this one ?
        var zoomTo = this.setZoomed();

        // Configre interpolator
        var zoomFactor = this.getZoomFactor(zoomTo);
        var vp = [zoomTo.x, zoomTo.y, zoomTo.r * zoomFactor]; //The center and width of the new "viewport"
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
                this.zoomCtx.scale = (this.rayon * 2) / interpolator(t)[2];

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
            //this.drawCanvasLight();
            //stats.end();
            if (finished) {
                this.isZooming = false;
                this.drawCanvas();
                this.drawCanvas(true);
                this.drawNodeHover(this.focusedNode, true);
                if (!elmHasBeenUpdated) this.nodeFocusedFromJs(this.focusedNode); // INIT
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

    colorToTextColor(color) { // @duplicate: exists as elm function.
        var c;
        if (["#7FDBFF" ,"#39CCCC" ,"#01FF70" ,"#FFDC00" ,"#AAAAAA" ,"#DDDDDD" ].includes(color.toUpperCase())) {
            c = "#000";
        } else {
            c = "#fff";
        }
        return c
    },

    getNodeColor(node, opac) {
        var z = this.zoomedNode || this.focusedNode;
        var color, depth;
        var depth = z.depth > 2 ? node.depth - z.depth + 2 : node.depth;
        opac = opac || "";

        // See doc here: https://www.w3resource.com/html5-canvas/html5-canvas-gradients-patterns.php
        var grd = this.ctx2d.createRadialGradient(node.ctx.centerX-node.ctx.rayon/4, node.ctx.centerY-node.ctx.rayon/2, 0,
            node.ctx.centerX, node.ctx.centerY, node.ctx.rayon);
        if (node.data.type_ === NodeType.Circle) {
            color = this.colorCircle(depth);
            grd.addColorStop(0, shadeColor(color, 10) + opac);
            grd.addColorStop(0.2, color + opac);
            grd.addColorStop(1, this.colorCircle(depth+1) + opac);
        } else if (node.data.type_ === NodeType.Role) {
            color = node.data.color || this.roleColors[node.data.role_type] || this.roleColors["_default_"];
            if (color.substring(0, 1) == "r") {
                grd = color;
            } else {
                grd.addColorStop(0, color+opac);
                grd.addColorStop(1, shadeColor(color, -20)+opac);
            }
        } else {
            console.warn("Node type unknonw", node.data.type_);
        }

        return grd
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
            } else if (dataNodes.filter(x => x.nameid === focusid) == 0) {
                console.warn("Focus node not found");
                focusid = null
            }
            graph = formatGraph(dataNodes);
            if (!graph) {
                console.warn("Could not load graph.");
            } else if (graph.length > 1) {
                console.warn("More than 1 graph given -> Some nodes are not connected.");
                if (!focusid) {
                    console.warn("no focus found, aborting.");
                    return
                }
                // Keep only the relevant tree
                // Get last parent thant contains focusid
                var root;
                var rootid = focusid;
                var i = 0; // infinite loop security
                while (rootid && i < 1000) {
                    root = dataNodes.find(x => x.nameid == rootid);
                    if (!root) break
                    rootid = root.parent ? root.parent.nameid : null;
                    i++;
                }
                if (root)
                    graph = graph.find(x => x.nameid === root.nameid);
            } else {
                graph = graph[0];
            }
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
            .size([this.rayon*2, this.rayon*2])
        (d3.hierarchy(graph)
            .sum(d => this.nodeSize(d, this.gStats))
            .sort(nodeOrder));

        this.nodesDict = Object.create(null);
        this.nodes = this.gPack.descendants(graph);
        this.circles_len = this.nodes.filter(x => x.data.type_ === NodeType.Circle).length
        this.roles_len = this.nodes.filter(x => x.data.type_ === NodeType.Role).length
        this.rootNode = this.nodes[0];
        this.hoveredNode = null;
        this.nodes.forEach(n => {
            // If the hidden canvas was send into this function and it does not yet have a color,
            // generate a unique one.
            n.colorid = this.genColor();
            this.colToCircle[n.colorid] = n;
            this.nodesDict[n.data.nameid] = n
        });
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
            // Assume node
            this.focusedNode = n;
        }

        // Fallback on Root on fails (e.g. Owner role)
        if (!this.focusedNode)
            this.focusedNode = this.rootNode

        if (!this.focusedNode) return
        this.setZoomed();

        if (setViewport) {
            this.vpOld = [this.zoomedNode.x, this.zoomedNode.y, this.zoomedNode.r * this.getZoomFactor(this.zoomedNode)];
        }

        return this.focusedNode
    },

    setZoomed() {
        var zoomTo;
        var focus = this.focusedNode;
        if (focus.parent && (focus.data.children === null || focus.data.children.length == 0)) {
            zoomTo = focus.parent;
        } else {
            zoomTo = focus;
        }
        this.zoomedNode = zoomTo;
        return this.zoomedNode
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
        return {mouseX: (e.clientX - r.left) , mouseY: (e.clientY - r.top)}
    },

    // Get the node under cursor in the canvas
    getNodeUnderPointer(e, p) {
        //Figure out where the mouse click occurred.
        if (!p) p = this.getPointerCtx(e);
        var hiddenCtx2d = this.hiddenCtx2d;

        // Get the corresponding pixel color on the hidden canvas and look up the node in our map.
        // This will return that pixel's color
        var pixel = hiddenCtx2d.getImageData(p.mouseX, p.mouseY, 1, 1).data;
        //Our map uses these rgb strings as keys to nodes.
        var color = "rgb(" + pixel[0] + "," + pixel[1] + ","+ pixel[2] + ")";
        var node = this.colToCircle[color];
        return node;
    },

    getParent(node) {
        return this.nodesDict[node.data.parent.nameid]
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
            } else if (node.data.role_type === RoleType.Bot) {
                rayon = node.r * this.rayonFactorBot;
            } else {
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

    // check geometrical condition
    // p: the mouse pointer
    // n: node pr element currently hovering
    // cond: what to test
    checkIf(p, cond, n) {
        var test = false;
        switch(cond) {
            case 'InCanvas':
                if (!this.$canvas) break
                var r = this.$canvas.getBoundingClientRect();
                var x2 = r.width;
                var y2 = r.height;
                test = (p.mouseX > 0) && (p.mouseY > 0) && (p.mouseX < x2) && (p.mouseY < y2);
                break
            case "InButtons":
                if (!n || !n.getBoundingClientRect || !this.$canvas) break
                var r = this.$canvas.getBoundingClientRect();
                var rBtn = n.getBoundingClientRect();
                var x1 = rBtn.left - r.left;
                var y1 = rBtn.top - r.top;
                var x2 = x1 + rBtn.width;
                var y2 = y1 + rBtn.height;
                test = (p.mouseX > x1) && (p.mouseY > y1) && (p.mouseX < x2) && (p.mouseY < y2);
                break
            case 'InTooltip':
                if (!n || !this.$tooltip) break
                // Intial version
                //var h = this.$tooltip.clientHeight +12;
                //var w = this.$tooltip.clientWidth/2 +6;
                //var x1 = n.ctx.centerX - w;
                //var x2 = n.ctx.centerX + w;
                //var y1 = n.ctx.centerY - n.ctx.rayon - h;
                //var y2;
                //if (n === this.focusedNode) {
                //    y2 = n.ctx.centerY - n.ctx.rayon*0.85;
                //} else {
                //    y2 = n.ctx.centerY - n.ctx.rayon*0.75;
                //}
                //test = (p.mouseX > x1) && (p.mouseX < x2) && (p.mouseY > y1) && (p.mouseY < y2);
                // --

                var h = this.$tooltip.clientHeight;
                var w = this.$tooltip.clientWidth/2 + h;
                var r = n.ctx.rayon;
                var x = {x: p.mouseX, y: p.mouseY}
                var a = {x: n.ctx.centerX, y: n.ctx.centerY + r}
                var b = {x: n.ctx.centerX - w, y: n.ctx.centerY - r - h}
                var c = {x: n.ctx.centerX + w, y: n.ctx.centerY - r - h}
                // First verify that the pointer is bear the border circle
                test = ((p.mouseX-n.ctx.centerX)**2 + (p.mouseY-n.ctx.centerY)**2 >= r**2)
                    && ptInTriangle(x, a, b, c)
                break
            case 'InFocus':
                var x = p.mouseX - this.focusedNode.ctx.centerX;
                var y = p.mouseY - this.focusedNode.ctx.centerY;
                test = x**2 + y**2 <= (this.focusedNode.ctx.rayon)**2 ;
                break
            case 'InZoomed':
                var x = p.mouseX - this.zoomedNode.ctx.centerX;
                var y = p.mouseY - this.zoomedNode.ctx.centerY;
                test = x**2 + y**2 <= (this.zoomedNode.ctx.rayon)**2 ;
                break
            case 'InVoid':
                var x = p.mouseX - this.rootNode.ctx.centerX;
                var y = p.mouseY - this.rootNode.ctx.centerY;
                // Security margin when movin to a tooltip that is outside the anchor
                test = x**2 + y**2 > (this.rootNode.ctx.rayon+10)**2 ;
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
        if (!node) return
        this.clearNodeHover();
        this.app.ports.nodeClickedFromJs.send(node.data.nameid);
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
        this.app.ports.nodeFocusedFromJs.send([node.data.nameid, this.gStats.maxdepth]);
    },

    sendNodeLeftClickFromJs(node) {
        this.app.ports.nodeLeftClickedFromJs.send(node.data.nameid);
    },

    sendNodeRightClickFromJs(node) {
        this.app.ports.nodeRightClickedFromJs.send(node.data.nameid);
    },

    //
    // Init
    //

    resizeMe() {
        if (!this.$canvas) return

        if (new Date() - this.rtime < this.delta) {
            setTimeout(() => this.resizeMe(), this.delta);
        } else {
            this.timeout = false;

            this.$canvasButtons.classList.add("is-invisible");
            this.$tooltip.classList.add("is-invisible");
            if (this.$welcomeButtons) this.$welcomeButtons.classList.add("is-invisible");

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

        // Not ready
        //this.loading()
        setTimeout(() => this.drawStargate(0, 1), 10);
    },

    drawStargate(radius, down) {
        this.clearCanvas(this.ctx2d);
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

        // Make it blink (consumes CPU for nothing really valuable)
        //sleep(333).then(() => {
        //    if (this.isLoading) {
        //        this.clearCanvas(this.ctx2d);
        //        this.drawStargate(radius+down, down);
        //    }
        //});
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

        // Setup Buttons
        this.$canvasButtons = document.getElementById('canvasButtons');
        this.$welcomeButtons = document.getElementById('welcomeButtons');

        // Setup nodeTooltip Tooltip
        this.$tooltip = document.getElementById('nodeTooltip');
        this.clearNodeTooltip()

        this.sizeDom();

        //
        // Create Circle Packing - GraphPack
        //

        this.nodeSize = this.nodeSizeTopDown;
        this.resetGraphPack(dataNodes, true, data.focusid);

        /*////////////////////////////////////////////////////////////
        ////////////////// Events Handler callback ///////////////////
        ////////////////////////////////////////////////////////////*/

        // Listen for clicks on the main canvas
        var nodeClickEvent = e => {
            if (this.isZooming) return false
            if (this.isFrozen) {
                this.isFrozen = false;
                return true
            }

            if (e.button === 0) {
                var p = this.getPointerCtx(e);
                if (!this.checkIf(p, "InZoomed")) {
                    // Go to parent
                    this.nodeClickedFromJs(this.focusedNode.parent);
                    return
                }

                var node = this.getNodeUnderPointer(e, p);
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

                if (isUpdated)
                    this.nodeClickedFromJs(node);

            } else if (e.button === 2) {
                //this.sendNodeRightClickFromJs(this.hoveredNode);
            }

            return false;
        };

        // Listen for mouse moves/hooverin on the main canvas
        var canvasMouseMoveEvent = e => {
            if (this.isZooming) return false
            if (this.isFrozen) return false
            var p = this.getPointerCtx(e);
            var node = this.getNodeUnderPointer(e, p);

            if (node) {
                if (node == this.hoveredNode) return
                if (node !== this.hoveredNode && this.checkIf(p, "InZoomed") && !this.checkIf(p, "InTooltip", this.hoveredNode))
                    this.drawNodeHover(node, true);
                else if (node !== this.hoveredNode && !this.checkIf(p, "InZoomed"))
                    this.drawNodeHover(this.focusedNode, true);
            } else if (this.hoveredNode != this.focusedNode) {
                // @DEBUG: there is a little dead zone between circle.
                // When it happens, it goes there and focused node receive the hover...
                // Or when not in the zoomed area
                //if (!this.checkIf(p, "InZoomed") || !this.checkIf(p, "InTooltip", this.hoveredNode))
                //    this.drawNodeHover(this.focusedNode, true);
                if (this.checkIf(p, "InVoid"))
                    this.drawNodeHover(this.focusedNode, true);
            } else {
                // nothing
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
            var node = this.getNodeUnderPointer(e);
            // Avoid redrawing and avoid glitch when leaving tooltip.
            if (node != this.hoveredNode && !this.checkIf(this.getPointerCtx(e), "InTooltip", this.hoveredNode)) {
                this.drawNodeHover(this.focusedNode, true);
            }

            return false
        }

        // Listen for mouse moves/hooverout on the main canvas
        var canvasMouseLeaveEvent = e => {
            var p = this.getPointerCtx(e);
            var isInCanvas = this.checkIf(p, "InCanvas"); // purpose of that is possibliy linked to issue #9232dcd
            if (!isInCanvas) {
                // Remove the node hover and border
                var clearBorder = this.hoveredNode && (this.hoveredNode != this.focusedNode);
                if (clearBorder) this.clearNodeHover();

                // Set the hover by default on the focused node
                this.drawNodeHover(this.focusedNode, true);
            } else {
                if (this.isFrozen) return false

                // Only show tooltip options/ellipsis on hoover
                //this.nodeHoveredFromJs(this.hoveredNode);
            }

            this.isFrozen = false;
            return false
        };

        // Catch right click context menu
        var contextMenuEvent = e => {
            if (!this.isFrozen && !this.isFrozenMenu) {
                e.preventDefault();
                this.sendNodeRightClickFromJs(this.hoveredNode);
                this.isFrozen = true;
                this.isFrozenMenu = true;
                return false
            } else {
                // do not works well
                e.preventDefault();
                this.isFrozenMenu = false;
                this.isFrozen = false;
                // Simulate on click to close the action panel
                var $c = document.getElementById(this.$tooltip.dataset.eventAction).querySelector(".clickMe");
                if ($c) {
                    $c.click();
                }
                return true
            }
        };

        // Mouse wheel To study
		//canvasMouseWheelEvent =  e => {
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
        //            this.nodeClickedFromJs(node);
        //        }
		//	}
		//}

        // Canvas button events redirection
        // Review -- Better implementation ?
        var canvasButtonsClick = e => {
            var p = this.getPointerCtx(e);
            var isInButtons = false;
            this.$canvasButtons.childNodes.forEach( o => {
                isInButtons |= this.checkIf(p, 'InButtons', o);
            });
            if (!isInButtons) {
                return nodeClickEvent(e)
            }
            return true
        };
        var canvasButtonsMove = e => {
            var p = this.getPointerCtx(e);
            var isInButtons = false;
            this.$canvasButtons.childNodes.forEach( o => {
                isInButtons |= this.checkIf(p, 'InButtons', o);
            });
            if (!isInButtons) {
                return canvasMouseMoveEvent(e)
            }
            return true
        };

        // Tooltip Clicks
        var tooltipTensionClick = e => {
            if (this.isFrozen) {
                this.isFrozen = false;
                return false
            }
            this.sendNodeLeftClickFromJs(this.hoveredNode);
            this.isFrozen = false;
            return true
        };
        var tooltipActionClick = e => {
            this.isFrozen = !this.isFrozen;
            return true
        };

        // On Resize handle
        window.onresize = () => {
            if (!this.focusedNode) return
            this.rtime = new Date();
            if (this.timeout === false) {
                this.timeout = true;

                // Smooth redraw
                setTimeout(() => this.resizeMe(), this.delta);
            }
        };

        //////////////////////////////////////////////////////////////
        /////////////////////// Initiate /////////////////////////////
        //////////////////////////////////////////////////////////////

        if (!this.graph) return
        console.log("Orga Canvas Initalization");
        this.isLoading = false;
        this.isZooming = false;
        this.isFrozen = false;
        this.isFrozenMenu = false;

		this.drawCanvas(true); // to add node.ctx

        //
        // Event listeners
        //

        var $subTooltipTension = document.getElementById(this.$tooltip.dataset.eventTension);
        var $subTooltipAction = document.getElementById(this.$tooltip.dataset.eventAction);

        // Cleanup old handlers to avoid dragons !
        for (var i=0; i<this.handlers.length; i++) {
            this.handlers[i][0].removeEventListener(this.handlers[i][1], this.handlers[i][2]);
        }

        this.handlers = [
            // Canvas mouse event
            [this.$canvas, "mousemove", canvasMouseMoveEvent],
            [this.$canvas, "mouseenter", canvasMouseEnterEvent],
            [this.$canvas, "mouseleave", canvasMouseLeaveEvent],
            [this.$canvas, "mousedown", nodeClickEvent],
            [this.$canvas, "contextmenu", contextMenuEvent],
            //[this.$canvas, "wheel", contextMenuEvent], // or "scroll" ?
            // Canvas buttons events
            [this.$canvasButtons, "mousedown", canvasButtonsClick],
            [this.$canvasButtons, "mousemove", canvasButtonsMove],
            // Tooltip events
            [$subTooltipTension, "mousedown", tooltipTensionClick],
            [$subTooltipAction, "mousedown", tooltipActionClick],
        ];

        // Setup handlers
        for (var i=0; i<this.handlers.length; i++) {
            this.handlers[i][0].addEventListener(this.handlers[i][1], this.handlers[i][2]);
        }

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

                if (this.hoveredNode) this.clearNodeHover();

                var focus = this.focusedNode

                this.resetGraphPack(this.graph, false);
                this.clearCanvas(this.ctx2d);
                this.clearCanvas(this.hiddenCtx2d);

                this.zoomToNode(this.rootNode, 0.9);
                setTimeout(() => {
                    this.zoomToNode(this.nodesDict[focus.data.nameid]);
                }, 50)
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
