/*
 * Fractale - Self-organisation for humans.
 * Copyright (C) 2026 Fractale Co
 *
 * This file is part of Fractale.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
 */

import { timer } from 'd3-timer'
import { interpolateZoom } from 'd3-interpolate'
import { easePolyInOut } from 'd3-ease'
import { hierarchy, packSiblings } from 'd3-hierarchy'
//import { scaleOrdinal } from 'd3-scale'
import { shadeColor, ptInTriangle } from './custom.js'


/*
 *
 * Graph Packing for Fractale Organisation
 *
 */

const d3 = Object.assign(
    {},
    {
        timer,
        interpolateZoom,
        easePolyInOut,
        hierarchy, packSiblings
        //scaleOrdinal,
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
const formatGraph = dataset => {
    var dataTree = [];
    var dataDict = Object.create(null);

    dataset.forEach((aData, i) => {
        dataDict[aData.nameid] = {
            ...aData,
            children: [],
            depth: 0
        }
    });

    dataset.forEach(aData => {
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

        if (aData.parent) {
            // Hidden parent: detach only the graph copy, keeping the snapshot intact for rebuilds.
            if (!dataDict[aData.parent.nameid]) {
                dataDict[aData.nameid].parent = null;
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

// Recursively set node.depth on the tree (root at 0) and return the tree height.
const computeDepth = (obj, depth = 0) => {
    obj.depth = depth;
    return (obj.children || []).reduce((m, c) => Math.max(m, computeDepth(c, depth + 1)), depth + 1)
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
    // @obsolete: Color computed in computeCircleColorRange()
    colorCircleRange: [],
    roleColors: {},
    usernameColor: "#8282cc",
    nameColor: "#474747",
    focusCircleColor: "#4a79ac", // blue>"#368ed3"
    focusCircleWidth: 4, // warning, can break stroke with canvas drawing.
    hoverCircleColor: "#555", //  grey-black>"#3f3f3f"
    hoverCircleWidth: 2,
    outsideZoomOpacity: "75",

    // Html element ID
    canvasParentId: "canvasParent",
    canvasId: "canvasOrga",

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
    circlesPadding: 4, // 1.8
    minCardsSide: 220, // below that, the in-circle action cards are not shown
    fontsizeCircle_start: 22,
    fontsizeRole_start: 19,
    fontstyleCircle: "Cantarell, Quicksand, Roboto, Lato, Ubuntu, Open Sans, Oxygen, sans-serif, fractaleicon",

    // Graph fx settings
    motionDuration: 400,
    zoomFactorRoot: 2.02,
    zoomFactorCircle: 2.02,
    zoomFactorRole: 6,
    zoomFactorGuest: 6,
    // rayon size of the node in the canvas
    rayonFactorRole: 0.95,
    rayonFactorGuest: 0.75,
    rayonFactorBot: 1,
    guestSizeDivider: 1,
    // y-axis offset for the top node
    nodeOffsetY: 0,

    // State
    rootNode: null, // The root node of the graph
    focusedNode: null, // The node that has the active focus
    zoomedNode: null, // The node that has is centered
    hoveredNode: null, // The node that is curently hoovered
    isFrozen: false, // Hover/tooltip locked on its node while its ActionPanel menu (or a modal from it) is open
    handlers: [],

    // Dragging (move a node by drag-and-drop)
    dragThreshold: 5, // px before a mousedown becomes a drag
    pressed: false, // left button pressed on the canvas
    dragCandidate: null, // {node, x, y} node pressed on, dragged once armed
    dragTarget: null,
    isDragging: false,

    // One motion owns both layout and viewport; node geometry is always the displayed frame.
    ease: d3.easePolyInOut.exponent(3),
    isZooming: false,
    motion: null,
    motionTimer: null,
    viewport: null,
    exitingNodes: [],
    initTimer: null,
    tooltipFrame: null,
    buttonsTimer: null,
    observer: null,

    // Resizing
    resizeTimer: null,
    delta: 200,
    userHeight: null, // canvas height set by the user with the resizer grip
    userColWidth: null, // canvas column width set by the user with the resizer grip

    // Html Elements
    $nextToChart: null,
    $canvas: null,
    $canvasButtons: null,
    $tooltip: null,
    // Canvas ctx
    ctx2d: null,

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

    //Clear canvas
    clearCanvas(ctx2d) {
        //var backgoundGrd = ctx2d.createLinearGradient(0, 0, this.width, 0);
        //backgoundGrd.addColorStop(0, this.colorDarker1);
        //backgoundGrd.addColorStop(1, this.colorDarker2);
        // fillRect: do not use rect()+fill(), it would accumulate subpaths on the persistent context.
        ctx2d.fillStyle = this.backgroundColor;
        ctx2d.fillRect(0, 0, this.width, this.height);
    },

    drawButtons() {
        var r = this.$canvas.getBoundingClientRect();
        var p = this.$canvasParent.getBoundingClientRect();
        var offsetLeft = r.left - p.left;
        var offsetTop = r.top - p.top;

        // Draw canvas buttons
        var buttonMargin = 13;
        this.$canvasButtons.style.height = this.height - 20 + "px";
        this.$canvasButtons.style.left = offsetLeft + r.width - this.$canvasButtons.offsetWidth - buttonMargin + "px";
        this.$canvasButtons.style.top = offsetTop + buttonMargin + "px";
        this.$canvasButtons.classList.remove("is-invisible");

        this.placeCanvasCards();

        // The tooltip stays hidden until its current node has been positioned.
    },

    // Fit the action cards box (Elm, empty focused circle only) on the focused circle; the cards draw themselves
    placeCanvasCards() {
        var $cards = document.getElementById('canvasCards');
        if (!$cards) return
        var node = this.focusedNode;
        if (!node || this.isZooming || !this.$canvas) return this.clearCanvasCards()
        this.addNodeCtx(node);
        var side = 2 * node.ctx.rayon;
        if (side < this.minCardsSide) return this.clearCanvasCards()
        var r = this.$canvas.getBoundingClientRect();
        var p = this.$canvasParent.getBoundingClientRect();
        $cards.style.width = $cards.style.height = side + "px";
        $cards.style.left = (node.ctx.centerX + (r.left - p.left) - side / 2) + "px";
        $cards.style.top = (node.ctx.centerY + (r.top - p.top) - side / 2) + "px";
        $cards.classList.remove("is-invisible");
    },

    clearCanvasCards() {
        var $cards = document.getElementById('canvasCards');
        if ($cards) $cards.classList.add("is-invisible");
    },

    // Size the canvas
    computeGeometry() {
        this.computedWidth = this.$canvasParent.offsetWidth; //var computedWidth = parseInt(window.getComputedStyle($canvasParent).width, 10);
        this.computedHeight = this.userHeight || (window.innerHeight) / 2;

        // Canvas settings
        this.width = Math.max(this.computedWidth - 4, this.minWidth);
        this.height = Math.max(this.computedHeight, this.minHeight); //(computedHeight > computedWidth ?  computedWidth: computedHeight );
        this.mobileSize = (window.innerWidth < 768 ? true : false);
        // On mobile the packing is bounded by the width: taller than wide is wasted space
        if (this.mobileSize && !this.userHeight) this.height = Math.min(this.height, this.width);

        this.rayon = (Math.min(this.width * 0.97, this.height * 0.97)) / 2;
        this.centerX = this.width / 2;
        this.centerY = this.height / 2;
        if (this.viewport) this.setViewport(this.viewport);
        else this.zoomCtx = { centerX: this.centerX, centerY: this.centerY, scale: 1 };
    },

    // Resize Html Elements created here
    sizeDom() {
        // Size Canvas -- backing store in device pixels, drawing done in CSS pixels via the DPR transform
        var dpr = window.devicePixelRatio || 1;
        this.$canvas.width = Math.round(this.width * dpr);
        this.$canvas.height = Math.round(this.height * dpr);
        this.$canvas.style.width = this.width + "px";
        this.$canvas.style.height = this.height + "px";
        // Setting width/height resets the context state, so re-apply the transform
        this.$canvas.getContext("2d").setTransform(dpr, 0, 0, dpr, 0, 0);

        // Size Element next to the canvas
        this.$nextToChart.style.minHeight = 1.5 * this.height + "px";
    },

    //The draw function of the canvas that gets called on each frame
    drawCanvas() {
        if (!this.graph) return

        this.clearCanvas(this.ctx2d);

        //Select our dummy nodes and draw the data to canvas.
        this.drawCurrent()
    },

    drawCurrent() {
        if (this.motion) {
            // Radius ordering keeps travelling children and exits above overlapping parents.
            var motion = this.motion;
            if (motion.changingRadii) motion.drawNodes.sort((a, b) => b.r - a.r || a.depth - b.depth);
            for (var n of motion.drawNodes) {
                if (n.opacity <= 0 || n.r <= 0) continue
                this.ctx2d.globalAlpha = n.opacity;
                this.drawNode(n);
            }
            this.ctx2d.globalAlpha = 1;
            this.drawFocusBorder(this.focusedNode);
            this.drawNodeNames(this.zoomedNode);
            return
        }
        // Separator between the opaque nodes and the other.
        var boundary = this.focusedNode;

        // First draw the node above the zoomedNode and their children (opacity)
        this.drawOutside(boundary);

        // Then draw the zoomedNode/focusedNode and descendends
        this.drawInside(boundary);

        // Draw names when zooming in/out
        this.drawNodeNames(this.zoomedNode)
    },

    drawOutside(b) {
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
                this.drawNode(tree[i], this.outsideZoomOpacity);
            }
        }
    },

    drawInside(b) {
        // list of nodes to draw.
        var tree = b.descendants();
        for (var i = 0; i < tree.length; i++) {
            let d = tree[i].depth - this.focusedNode.depth
            if (d >= 0 && d < 4 || tree[i].depth == 0) {
                this.drawNode(tree[i]);
            }
        }

        this.drawFocusBorder(b);
    },

    drawFocusBorder(b) {
        this.addNodeCtx(b);
        if (b.ctx.rayon <= 0) return
        var ctx = this.ctx2d;
        ctx.globalAlpha = b.opacity ?? 1;
        var w = this.focusCircleWidth;
        var color = this.focusCircleColor;
        ctx.beginPath();
        ctx.arc(b.ctx.centerX, b.ctx.centerY, b.ctx.rayon + 0.1 + w * 0.5,
            0, 2 * Math.PI, true);
        ctx.lineWidth = w;
        ctx.strokeStyle = color;
        ctx.stroke();
        ctx.globalAlpha = 1;
    },

    drawNode(node, opac) {
        var ctx = this.ctx2d;
        var circleColor;

        this.addNodeCtx(node);

        // Get the circle Color
        if (opac && (opac[0] == "#" || !opac.length)) {
            // Given color OR given colorGradient (ot length)
            circleColor = opac;
        } else {
            circleColor = this.getNodeColor(node);
        }

        // Draw node
        var rayon = node.ctx.rayon;
        switch (node.data.type_) {
            case NodeType.Role:
                //var r = node.ctx.rayon*0.75;
                //var l = node.ctx.rayon/20;
                //ctx.beginPath();
                //ctx.fillStyle = circleColor;
                //ctx.ellipse(node.ctx.centerX, node.ctx.centerY, r+l, r-l, 0, 0, Math.PI * 2, false);
                //ctx.ellipse(node.ctx.centerX, node.ctx.centerY, r-l, r+l, 0, 0, Math.PI * 2, false);
                //ctx.fill();
                //var rayon = Math.min(node.ctx.rayon, 25);
                ctx.beginPath();
                ctx.fillStyle = circleColor;
                ctx.arc(node.ctx.centerX, node.ctx.centerY, rayon, 0, 2 * Math.PI, true);
                ctx.fill();
                break;
            default:
                ctx.beginPath();
                ctx.fillStyle = circleColor;
                ctx.arc(node.ctx.centerX, node.ctx.centerY, rayon, 0, 2 * Math.PI, true);
                ctx.fill();
        }

        // Draw owned Role
        if (rayon > 1 && (this.uctx && node.data.first_link) && this.uctx.username == node.data.first_link.username) {
            // Draw user pin
            //var r =  Math.max(10 - (node.depth - this.focusedNode.depth) , 1)/4
            //ctx.beginPath();
            //ctx.fillStyle = "green";
            //ctx.arc(node.ctx.centerX, node.ctx.centerY + node.ctx.rayon*3/4 , r,
            //    0, 2 * Math.PI, true);
            //ctx.fill();

            // Draw user dashed border
            var w = 2;
            var color = this.link2Color;
            ctx.beginPath();
            ctx.setLineDash([10, 10]);
            ctx.beginPath();
            ctx.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon - w * 0.5,
                0, 2 * Math.PI, true);
            ctx.lineWidth = w;
            ctx.strokeStyle = color;
            ctx.stroke();
            ctx.setLineDash([]);
        }
    },

    drawNodeNames(node) {
        var ctx = this.ctx2d;
        var n, opac;
        var defOpac = (node == this.focusedNode)

        if (node.data.children.length > 99) {
            // Do not print names in cercle too many child
            return
        }
        for (var i = 0; i < node.data.children.length; i++) {
            n = node.children[i];
            if (!n.ctx || node.depth !== n.depth - 1) continue
            if (this.motion && (!n.opacity || n.ctx.rayon < 8)) continue
            ctx.globalAlpha = this.motion ? n.opacity : 1;

            // Draw names
            if (defOpac) {
                opac = "ff";
            } else if (n == this.focusedNode) {
                opac = "ad";
            } else {
                opac = "80";
            }
            if (n.data.type_ === NodeType.Circle) {
                this.drawCircleName(n, opac)
            } else {
                this.drawRoleName(n, opac)
            }
        }
        ctx.globalAlpha = 1;
    },

    drawCircleName(node, opac) {
        // Draw circle names.
        var ctx2d = this.ctx2d
        var fontSize = this.fontsizeCircle_start;
        var text, textWidth;

        // @debug ME
        opac = "";

        // Name
        text = node.data.name;
        textWidth = ctx2d.measureText(text).width;
        if (textWidth > node.ctx.rayon * 2.5) {
            text = text.split(" ").map(s => s.substring(0, 3)).join("·")
            textWidth = ctx2d.measureText(text).width;
            if (textWidth > node.ctx.rayon * 2.5) {
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
        //ctx2d.strokeStyle = "#5e6d6f" + opac;
        ctx2d.fillStyle = this.nameColor + opac;
        if (node.data.visibility !== this.getParent(node).data.visibility) {
            if (node.data.visibility == NodeVisibility.Public) {
                // icon-globe
                text = "\ue960 " + text;
            } else if (node.data.visibility == NodeVisibility.Private) {
                // icon-lock
                text = "\ue930 " + text;
            } else if (node.data.visibility == NodeVisibility.Secret) {
                // icon-key
                text = "\ue95f " + text;
            }
        }
        //ctx2d.strokeText(text, node.ctx.centerX, node.ctx.centerY - node.ctx.rayon * 0.4);
        ctx2d.fillText(text, node.ctx.centerX, node.ctx.centerY - node.ctx.rayon * 0.4);
        ctx2d.fill();
        //ctx2d.stroke();

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

        // @debug ME
        opac = "";

        // Name
        text = node.data.name;
        var textWidth = ctx2d.measureText(text).width;
        if (textWidth + textHeight / 2 > node.ctx.rayon * 2) {
            text = text.split(" ").map(s => s.substring(0, 3)).join("·")
            textWidth = ctx2d.measureText(text).width;
            if (textWidth + textHeight / 2 > node.ctx.rayon * 2) {
                text = text.split("·").map(s => s.substring(0, 1)).join("·")
            }
        }

        // final
        if (text) {
            // Upper case the first char
            text = text[0].toUpperCase() + text.slice(1);
            ctx2d.beginPath();
            ctx2d.font = fontSize + "px " + this.fontstyleCircle;
            ctx2d.textAlign = "center";
            // Color
            var roleTextColor;
            if (node.data.color) {
                roleTextColor = this.colorToTextColor(node.data.color);
            } else {
                roleTextColor = this.nameColor + opac;
            }
            ctx2d.fillStyle = roleTextColor;
            ctx2d.fillText(text, node.ctx.centerX, node.ctx.centerY);
            ctx2d.fill();
            // Icon Tips
            ctx2d.fillStyle = roleTextColor;
            if (node.data.role_type == RoleType.Bot) {
                //ctx2d.fillText('🤖', node.ctx.centerX, node.ctx.centerY-node.ctx.rayon*0.5);
                ctx2d.fillText('\ue962', node.ctx.centerX, node.ctx.centerY - node.ctx.rayon * 0.45);
            } else if (node.data.type_ == NodeType.Role) {
                // Role type icon
                if (node.data.role_type == RoleType.Coordinator) {
                    ctx2d.fillText('\ue963', node.ctx.centerX, node.ctx.centerY - node.ctx.rayon * 0.45);
                } else if (node.data.role_type == RoleType.Owner) {
                    ctx2d.fillText('\ue964', node.ctx.centerX, node.ctx.centerY - node.ctx.rayon * 0.45);
                } else {
                    ctx2d.fillText('\uf06c', node.ctx.centerX, node.ctx.centerY - node.ctx.rayon * 0.45);
                }
            }
            ctx2d.fill();

            // Username
            if (node.data.first_link) {
                var text_username = null;
                ctx2d.font = fontSize - 7 + "px " + this.fontstyleCircle;
                text_username = "@" + node.data.first_link.username;
                textWidth = ctx2d.measureText(text_username).width;
                if (textWidth > node.ctx.rayon * 2)
                    text_username = "@"

                ctx2d.beginPath();
                ctx2d.fillStyle = this.usernameColor;
                ctx2d.fillText(text_username, node.ctx.centerX, node.ctx.centerY + node.ctx.rayon * 0.4);
                ctx2d.fill();
            }
        }
    },

    // Draw node border + eventually tooltip
    drawNodeHover(node, doDrawTooltip) {
        if (this.isFrozen) node = this.hoveredNode;
        if (this.isZooming || !node) return
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
            ctx2d.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon + 0.1 + w / 2, 0, 2 * Math.PI, true);
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

    // Redraw the graph with the drag ghost and the highlighted drop target
    drawDragFeedback(p) {
        this.hoveredNode = null;
        this.drawCanvas();
        var ctx2d = this.ctx2d;
        var target = this.dragTarget;

        // Drop target border
        if (target && target.ctx) {
            var w = this.hoverCircleWidth;
            ctx2d.beginPath();
            ctx2d.setLineDash([6, 4]);
            ctx2d.lineWidth = w * 2;
            ctx2d.strokeStyle = this.hoverCircleColor;
            ctx2d.arc(target.ctx.centerX, target.ctx.centerY, target.ctx.rayon + 0.1 + w, 0, 2 * Math.PI, true);
            ctx2d.stroke();
            ctx2d.setLineDash([]);
        }

        // Ghost of the dragged node under the pointer
        var source = this.dragCandidate.node;
        var rayon = Math.min(source.ctx ? source.ctx.rayon : 20, 25);
        ctx2d.beginPath();
        ctx2d.globalAlpha = 0.6;
        ctx2d.fillStyle = this.getNodeColor(source);
        ctx2d.arc(p.mouseX, p.mouseY, rayon, 0, 2 * Math.PI, true);
        ctx2d.fill();
        ctx2d.globalAlpha = 1;
    },

    // Reset the drag state and repaint the graph
    endDrag() {
        var wasDragging = this.isDragging;
        this.pressed = false;
        this.dragCandidate = null;
        this.dragTarget = null;
        this.isDragging = false;
        if (this.$canvas) this.$canvas.style.cursor = "";
        if (wasDragging) {
            this.hoveredNode = null;
            this.drawCanvas();
            this.drawNodeHover(this.focusedNode, false);
        }
    },

    // Clean node hovering
    clearNodeHover() {
        if (this.isZooming || !this.hoveredNode) return

        // Remove the circle border
        var node = this.hoveredNode;
        var w;
        if (node == this.focusedNode) w = this.focusCircleWidth;
        else w = this.hoverCircleWidth;
        this.ctx2d.beginPath();
        this.ctx2d.lineWidth = w * 1.5;
        this.ctx2d.strokeStyle = this.getNodeColor(node.parent || this.rootNode);
        this.ctx2d.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon + 0.1 + w / 2, 0, 2 * Math.PI, true);
        this.ctx2d.stroke();

        // Fix canvas alteration (text cutted and opacity stacked)
        if (this.focusedNode == this.zoomedNode)
            this.drawInside(this.focusedNode);
        else {
            // Redraw zommed node with the color of its parent.
            //1) This allow opacity color to be consitent !
            var p = this.zoomedNode.parent;
            // First reset the node colors
            this.drawNode(this.zoomedNode, this.backgroundColor);
            //2) Then redraw the inner circle with its parents colors.
            for (var j = 0; j < this.zoomedNode.depth; j++) {
                var color = this.getNodeColor(p);
                this.drawNode(this.zoomedNode, color);
                p = p.parent;
            }

            // Redraw the zommed node before redrawing names
            var tree = [this.zoomedNode, ...this.zoomedNode.children]
            for (var i = 0; i < tree.length; i++) {
                this.drawNode(tree[i], this.outsideZoomOpacity);
            }

            this.drawInside(this.focusedNode);
        }
        this.drawNodeNames(this.zoomedNode);

        // Update context
        this.hoveredNode = null; //@debug: use globCtx
        return
    },

    // Draw the node tooltip
    drawNodeTooltip(node) {
        cancelAnimationFrame(this.tooltipFrame);
        this.$tooltip.inert = true;
        this.nodeHoveredFromJs(node);
        // Elm queues its render on the hover port; measure its options in the same frame, afterwards.
        this.tooltipFrame = requestAnimationFrame(() => {
            this.tooltipFrame = null;
            if (this.isActive() && !this.isZooming && !this.isDragging && this.hoveredNode === node)
                this.drawNodeTooltip_(node);
        });
    },
    drawNodeTooltip_(node) {
        var $tooltip = this.$tooltip
        var $subTooltip = document.getElementById(this.$tooltip.dataset.eventTension);
        if (!$subTooltip) return
        $subTooltip.childNodes[0].textContent = node.data.name;
        // Visibility-hidden elements remain measurable; reveal only after positioning.
        // -- Position relative to canvasParent (the positioned ancestor)
        var r = this.$canvas.getBoundingClientRect();
        var p = this.$canvasParent.getBoundingClientRect();
        var offsetLeft = r.left - p.left;
        var offsetTop = r.top - p.top;
        var tw = $tooltip.clientWidth;
        var l = (node.ctx.centerX + offsetLeft - (tw / 2 + 1));
        if (node === this.focusedNode && node === this.zoomedNode) {
            // below the circle
            var hw = (-$tooltip.clientHeight + 2 * node.ctx.rayon);
            var t = (node.ctx.centerY + offsetTop - (hw / 2 + 23));
        } else {
            // above the circle
            var hw = ($tooltip.clientHeight + 2 * node.ctx.rayon);
            var t = (node.ctx.centerY + offsetTop - (hw / 2 + 23));
        }

        if (l + tw / 2 - offsetLeft < 0 || offsetLeft + r.width - tw / 2 - l < 0) {
            // the tooltip overflow "too much" outside the canvas. (left/right
            this.clearNodeTooltip();
            return
        } else if (t + $tooltip.clientHeight / 3 - offsetTop < 0) {
            // Overflow on top
            var hw = (-$tooltip.clientHeight / 2 + 2 * node.ctx.rayon);
            var t = (node.ctx.centerY + offsetTop - (hw / 2 + 23));
        }
        $tooltip.style.left = l + "px";
        $tooltip.style.top = t + "px";
        // Settle hidden placement before enabling transitions; visible replacements glide from the current frame.
        if ($tooltip.classList.contains("is-invisible")) $tooltip.getBoundingClientRect();
        $tooltip.classList.remove("is-invisible");
        $tooltip.style.pointerEvents = "";
        $tooltip.inert = false;

        return
    },

    // Clear node tooltip.
    clearNodeTooltip() {
        cancelAnimationFrame(this.tooltipFrame);
        this.tooltipFrame = null;
        if (this.$tooltip) {
            this.$tooltip.classList.add("is-invisible");
            this.$tooltip.style.pointerEvents = "none";
            this.$tooltip.inert = true;
        }
    },
    clearContextMenu() {
        this.isFrozen = false;
    },


    setViewport(vp) {
        this.viewport = vp;
        this.zoomCtx = { centerX: vp[0], centerY: vp[1], scale: this.rayon * 2 / vp[2] };
    },

    visibleNodes() {
        var b = this.focusedNode;
        if (!b) return []
        var outside = b.parent ? [...b.parent.ancestors().slice(0, 2), ...b.parent.children] : [];
        return [...new Set([...outside, ...b.descendants().filter(n => n.depth - b.depth < 4)])];
    },

    cancelMotion() {
        if (this.motionTimer) this.motionTimer.stop();
        this.motionTimer = null;
        this.motion = null;
        this.isZooming = false;
    },

    startMotion(layout = false, immediate = false) {
        this.cancelMotion();
        this.endDrag();
        this.hoveredNode = null;
        this.clearContextMenu();
        this.clearNodeTooltip();
        this.clearCanvasCards();
        this.nodeHoveredFromJs(null);

        var visible = new Set(this.visibleNodes());
        var tracks = [];
        this.nodes.forEach(node => {
            if (!visible.has(node) && !(node.opacity > 0)) {
                Object.assign(node, node.target);
                node.opacity = 0;
                node.ctx = null;
                return
            }
            tracks.push({
                node,
                from: { x: node.x, y: node.y, r: node.r, opacity: node.opacity ?? 0 },
                to: { ...node.target, opacity: visible.has(node) ? 1 : 0 },
            });
        });
        tracks.push(...this.exitingNodes.map(node => ({
            node,
            from: { x: node.x, y: node.y, r: node.r, opacity: node.opacity },
            to: { x: node.x, y: node.y, r: 0, opacity: 0 },
        })));
        var z = this.zoomedNode.target;
        var vp = [z.x, z.y, z.r * this.getZoomFactor(this.zoomedNode)];
        var from = this.viewport || vp;
        var interpolate = layout ? t => from.map((v, i) => v + (vp[i] - v) * t) : d3.interpolateZoom(from, vp);
        // Compare displayed geometry, not the request type: zoom may interrupt a reflow.
        tracks.forEach(track => {
            var { from, to } = track;
            track.geometry = from.x !== to.x || from.y !== to.y || from.r !== to.r;
        });
        var changingRadii = tracks.some(({ from, to }) => from.r !== to.r);
        var drawNodes = tracks.map(t => t.node).sort((a, b) => b.r - a.r || a.depth - b.depth);
        var motion = { tracks, drawNodes, changingRadii, interpolate, vp };
        this.motion = motion;
        this.isZooming = true;
        var reduced = window.matchMedia?.('(prefers-reduced-motion: reduce)');
        if (immediate || reduced?.matches) {
            this.stepMotion(1);
            return
        }
        this.stepMotion(0);
        this.motionTimer = d3.timer(elapsed => {
            if (this.motion !== motion) return
            if (!this.isActive()) { this.dispose(); return }
            this.stepMotion(reduced?.matches ? 1 : Math.min(1, elapsed / this.motionDuration));
        });
    },

    stepMotion(progress) {
        var motion = this.motion;
        if (!motion) return
        var t = this.ease(Math.max(0, Math.min(1, progress)));
        motion.tracks.forEach(({ node, from, to, geometry }) => {
            if (geometry) {
                node.x = t === 1 ? to.x : from.x + (to.x - from.x) * t;
                node.y = t === 1 ? to.y : from.y + (to.y - from.y) * t;
                node.r = t === 1 ? to.r : from.r + (to.r - from.r) * t;
            }
            if (from.opacity !== to.opacity)
                node.opacity = t === 1 ? to.opacity : from.opacity + (to.opacity - from.opacity) * t;
            node.ctx = null;
        });
        this.setViewport(t === 1 ? motion.vp : motion.interpolate(t));
        if (t === 1) {
            this.cancelMotion();
            this.exitingNodes = [];
        }
        this.drawCanvas();
        if (t === 1) {
            this.drawNodeHover(this.focusedNode, true);
            this.placeCanvasCards();
        }
    },

    normalizeFocusId(focusid) {
        if (typeof focusid !== 'string' || this.nodesDict?.[focusid]) return focusid;
        try { return decodeURIComponent(focusid); } catch (_) { return focusid; }
    },

    zoomToNode(focus) {
        if (!this.focusedNode || !this.isActive()) return
        focus = this.normalizeFocusId(focus);
        if (typeof focus === 'string' && !this.nodesDict[focus] && this.dataNodes.some(n => n.nameid === focus)) {
            this.resetGraphPack(this.dataNodes, focus);
            return
        }
        var previous = this.focusedNode;
        this.setFocus(focus);
        // Route reinitializations must not restart an in-flight layout for the same focus.
        if (previous !== this.focusedNode) this.startMotion();
        else if (!this.isZooming) this.drawNodeHover(this.focusedNode, true);
        this.nodeFocusedFromJs(this.focusedNode);
    },

    //
    // D3/GraphPack
    //

    // Determine the leaf (role) weight in the circle packing; its radius is sqrt(weight).
    nodeSizeTopDown(n, stats) {
        //var dvd = 1;
        //if (n.role_type == RoleType.Guest) {
        //    dvd = this.guestSizeDivider;
        //} else if (n.role_type == RoleType.Bot) {
        //     dvd = 1;
        //}

        var v = 2000;
        if (n.type_ == "Role" || n.type_ == "Hidden") {
            if (n.role_type == RoleType.Bot) {
                v = v / 12;
            } else {
                v = v / 4;
            }
        }

        return v / (n.depth + 1) ** 3
    },

    nodeSizeBottomUp(n, stats) {
        var dvd = (n.role_type == RoleType.Guest) ? this.guestSizeDivider : 1;
        var sizeDefault = 4;
        return 10000 / (stats.maxdepth) ** (Math.max(0, sizeDefault - n.depth)) / dvd
        //return v ** (n.depth+1)
    },

    computeCircleColorRange() {
        var styles = getComputedStyle(document.documentElement);
        // @DEBUG: CSS color maybe be changed to string by webpack minimizer (I suppose)
        // which breaks the opacity logics in getNodeColor().
        this.colorCircleRange = [
            styles.getPropertyValue('--gp-lvl-0-bg').trim(),
            styles.getPropertyValue('--gp-lvl-1-bg').trim(),
            styles.getPropertyValue('--gp-lvl-2-bg').trim(),
            styles.getPropertyValue('--gp-lvl-3-bg').trim(),
            styles.getPropertyValue('--gp-lvl-4-bg').trim(),
            styles.getPropertyValue('--gp-lvl-5-bg').trim(),
            styles.getPropertyValue('--gp-lvl-6-bg').trim(),
            styles.getPropertyValue('--gp-lvl-7-bg').trim(),
        ]
        // Create the roleColors map dynamically
        this.roleColors = Object.keys(RoleType).reduce((colors, role) => {
          // Convert role name to lowercase for CSS variable naming convention
          const cssVarName = `--${role.toLowerCase()}`;

          // Try to get the CSS variable value, fall back to default if not found
          const colorValue = styles.getPropertyValue(cssVarName).trim() || "#a2b9df";

          // Add to the colors object
          colors[RoleType[role]] = colorValue;
          return colors;
        }, {
          // Add the default color
          "_default_": "#a2b9df"
        });

        this.backgroundColor = styles.getPropertyValue('--body-background-color').trim()
        this.focusCircleColor = styles.getPropertyValue('--link').trim()
        this.hoverCircleColor = styles.getPropertyValue('--text-weak').trim()
        this.link2Color = styles.getPropertyValue('--link2').trim()
        this.nameColor = styles.getPropertyValue('--text-evidence').trim()
        this.usernameColor = styles.getPropertyValue('--text').trim()
    },

    // Mapping function from a node depth to color.
    colorCircle(k) {
        //d3.scaleOrdinal()
        //.domain(Array.from({length:this.colorCircleRange.length},(v,k)=>k%this.colorCircleRange.length))
        //.range(this.colorCircleRange)
        //.unknown(this.backgroundColor);
        return this.colorCircleRange[k % this.colorCircleRange.length]
    },

    colorToTextColor(color) { // @duplicate: exists as elm function.
        var c;
        if (["#7FDBFF", "#39CCCC", "#01FF70", "#FFDC00", "#AAAAAA", "#DDDDDD"].includes(color.toUpperCase())) {
            c = "#000";
        } else {
            c = "#fff";
        }
        return c
    },

    getNodeColor(node) {
        var z = this.zoomedNode || this.focusedNode;
        var depth = Math.max(0, z.depth > 2 ? node.depth - z.depth + 2 : node.depth);
        var color;
        if (node.data.type_ === NodeType.Circle) {
            color = this.colorCircle(depth);
        } else if (node.data.type_ === NodeType.Role) {
            color = node.data.color || this.roleColors[node.data.role_type] || this.roleColors["_default_"];
        } else {
            console.warn("Node type unknonw", node.data.type_);
        }
        return color
    },

    getZoomFactor(node) {
        var zoomFactor;
        if (node.data.type_ === NodeType.Role) {
            if (node.data.role_type == "Guest") {
                zoomFactor = this.zoomFactorGuest;
            } else {
                zoomFactor = this.zoomFactorRole;
            }
        } else if (node.data.parent == undefined) {
            zoomFactor = this.zoomFactorRoot;
        } else if (node.children && node.children.length >= 10) {
            zoomFactor = this.zoomFactorRoot;
        } else {
            zoomFactor = this.zoomFactorCircle;
        }
        return zoomFactor
    },

    // Init and create the GraphPack data structure
    resetGraphPack(dataNodes, focusid, nodeRenames = {}) {
        const ids = new Set(dataNodes.map(n => n.nameid));
        if (!ids.has(focusid)) focusid = this.normalizeFocusId(focusid);
        // Server metadata refreshes must not restart an unchanged layout or consume its exits.
        var layoutKey = nodes => JSON.stringify(nodes.map(n => [n.nameid, n.parent?.nameid, n.name, n.type_, n.role_type]));
        if (this.graph && (!focusid || this.nodesDict[focusid]) && this.packedNodeSize === this.nodeSize && layoutKey(dataNodes) === layoutKey(this.dataNodes)) {
            this.dataNodes = dataNodes;
            dataNodes.forEach(data => {
                var node = this.nodesDict[data.nameid];
                if (node) Object.assign(node.data, data);
            });
            this.uctx = JSON.parse(localStorage.getItem("user_ctx"));
            this.drawCanvas();
            this.zoomToNode(focusid);
            return
        }
        var oldNodes = new Map([...this.exitingNodes, ...(this.nodes || [])]
            .map(n => [nodeRenames[n.data.nameid] || n.data.nameid, n]));
        var oldFocusPath = this.focusedNode ? this.focusedNode.ancestors().map(n => nodeRenames[n.data.nameid] || n.data.nameid) : [];
        this.computeCircleColorRange();
        if (!ids.has(focusid)) focusid = oldFocusPath.find(id => ids.has(id));
        var graph = formatGraph(dataNodes);
        if (graph.length > 1) {
            // Private parents may be absent: keep the connected tree containing the focus.
            var root = dataNodes.find(n => n.nameid === focusid);
            var i = 0;
            while (root && root.parent && ids.has(root.parent.nameid) && i++ < 1000)
                root = dataNodes.find(n => n.nameid === root.parent.nameid);
            graph = graph.find(n => root && n.nameid === root.nameid) || graph[0];
        } else {
            graph = graph[0];
        }
        if (!graph) {
            // An authoritative empty/unusable snapshot must not leave old authorized data visible.
            this.dispose();
            this.init_canvas();
            this.nodeHoveredFromJs(null);
            return
        }

        // Compute global statistics
        this.gStats = { maxdepth: computeDepth(graph) };

        // Compute circle packing
        this.gPack = this.packGraph(d3.hierarchy(graph));

        this.cancelMotion();
        this.nodesDict = Object.create(null);
        this.nodes = this.gPack.descendants();
        this.rootNode = this.nodes[0];
        this.nodes.forEach(n => {
            this.nodesDict[n.data.nameid] = n;
            n.target = { x: n.x, y: n.y, r: n.r };
            var old = oldNodes.get(n.data.nameid);
            n.opacity = old ? old.opacity : 0;
            if (old) {
                n.x = old.x;
                n.y = old.y;
                n.r = old.r;
                oldNodes.delete(n.data.nameid);
            } else if (this.viewport) n.r = 0;
        });
        this.exitingNodes = [...oldNodes.values()].filter(n => n.opacity > 0 && n.r > 0);
        this.graph = graph;
        this.dataNodes = dataNodes;
        this.packedNodeSize = this.nodeSize;
        this.setFocus(this.nodesDict[focusid] ? focusid : oldFocusPath.find(id => this.nodesDict[id]));
        this.uctx = JSON.parse(localStorage.getItem("user_ctx"));
        this.startMotion(true, !this.viewport);
        this.nodeFocusedFromJs(this.focusedNode);
    },

    // Minimum radius of a circle: the footprint of two of its (would-be) roles.
    minCircleRayon(node, pad) {
        var u = Math.sqrt(this.nodeSize({ type_: NodeType.Role, role_type: RoleType.Peer, depth: node.depth + 1 }, this.gStats));
        return 2 * (u + pad) + pad
    },

    // Bottom-up packing: a circle encloses its packed children but never shrinks below
    // minCircleRayon, so sparse circles (empty, one role, two roles) keep the same size.
    // Siblings are ordered by kind then drawn size, so a rename cannot reshape a circle.
    packLayout(node, pad) {
        if (node.children) {
            node.children.forEach(c => this.packLayout(c, pad));
            node.children.sort(this.nodeOrder);
            var cs = node.children;
            cs.forEach(c => c.r += pad);
            // packSiblings centers the arrangement on its enclosing circle.
            d3.packSiblings(cs);
            node.r = cs.reduce((r, c) => Math.max(r, Math.hypot(c.x, c.y) + c.r), 0) + pad;
            cs.forEach(c => c.r -= pad);
        } else {
            node.r = node.data.type_ === NodeType.Circle ? 0 : Math.sqrt(this.nodeSize(node.data, this.gStats));
        }
        if (node.data.type_ === NodeType.Circle) node.r = Math.max(node.r, this.minCircleRayon(node, pad));
        return node.r
    },

    // Circles first (largest first), then roles; names and IDs break size ties.
    nodeOrder(n1, n2) {
        const typeOrder = { Circle: 0, Role: 1 };
        return typeOrder[n1.data.type_] - typeOrder[n2.data.type_]
            || n2.r - n1.r
            || n1.data.name.localeCompare(n2.data.name)
            || (n1.data.nameid < n2.data.nameid ? -1 : n1.data.nameid > n2.data.nameid ? 1 : 0)
    },

    // Lay out the hierarchy, then scale it to fill the canvas (padding stays ~circlesPadding px at root).
    packGraph(root) {
        this.packLayout(root, 0);
        this.packLayout(root, this.circlesPadding * root.r / (this.rayon * 2));
        var k = this.rayon / root.r;
        root.x = root.y = this.rayon;
        root.eachBefore(n => {
            n.r *= k;
            if (n.parent) {
                n.x = n.parent.x + k * n.x;
                n.y = n.parent.y + k * n.y;
            }
        });
        return root
    },

    setFocus(n) {
        if (!n) {
            // focus on root node by default
            this.focusedNode = this.rootNode;
        } else if (typeof (n) === 'string') {
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

        return this.focusedNode
    },

    setZoomed() {
        var zoomTo;
        var focus = this.focusedNode;
        if (focus.parent && !focus.children) {
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

    // Get the mouse coordinate whithin the canvas reference.
    getPointerCtx(e) {
        var r = this.$canvas.getBoundingClientRect();
        return { mouseX: (e.clientX - r.left), mouseY: (e.clientY - r.top) }
    },

    // Drawn radius of a node in pack coordinates (roles are shrunk by type).
    nodeRayon(node) {
        if (node.data.type_ === NodeType.Role) {
            if (node.data.role_type === RoleType.Guest) return node.r * this.rayonFactorGuest
            if (node.data.role_type === RoleType.Bot) return node.r * this.rayonFactorBot
            return node.r * this.rayonFactorRole
        }
        return node.r
    },

    // Test if a point, in graph (pack) coordinates, falls within a node's drawn circle.
    nodeContains(node, gx, gy) {
        var r = this.nodeRayon(node);
        return (gx - node.x) ** 2 + (gy - node.y) ** 2 <= r ** 2
    },

    // Get the node under cursor by geometric hit-testing.
    // Note: do not use pixel picking here (hidden canvas + getImageData), as browsers
    // with fingerprinting protection (e.g. Brave) add noise to canvas readbacks.
    getNodeUnderPointer(e, p) {
        if (!p) p = this.getPointerCtx(e);
        if (this.isZooming || !this.rootNode || !this.focusedNode || !this.zoomCtx) return undefined

        // Pointer position in graph (pack) coordinates -- inverse of addNodeCtx.
        var gx = (p.mouseX - this.centerX) / this.zoomCtx.scale + this.zoomCtx.centerX;
        var gy = (p.mouseY - this.centerY - this.nodeOffsetY) / this.zoomCtx.scale + this.zoomCtx.centerY;
        if (!this.nodeContains(this.rootNode, gx, gy)) return undefined

        // Walk down the hierarchy, mirroring what drawCurrent renders: circles are
        // drawn up to 3 levels below the focused node inside its subtree, and only
        // down to the focused node's siblings outside of it.
        var node = this.rootNode;
        var inFocusPath = (node === this.focusedNode);
        while (node.children && node.data.type_ === NodeType.Circle) {
            var next = null;
            for (var i = 0; i < node.children.length; i++) {
                var c = node.children[i];
                if (this.nodeContains(c, gx, gy)) { next = c; break }
            }
            var maxDepth = inFocusPath ? this.focusedNode.depth + 3 : this.focusedNode.depth;
            if (!next || next.depth > maxDepth) break
            node = next;
            if (node === this.focusedNode) inFocusPath = true;
        }
        return node;
    },

    // Valid drop target: a circle that is neither the dragged node's parent nor part of its own subtree.
    getDropTarget(e, p) {
        var node = this.getNodeUnderPointer(e, p);
        var source = this.dragCandidate && this.dragCandidate.node;
        if (!node || !source) return null
        if (node.data.type_ !== NodeType.Circle) return null
        if (node === source.parent) return null
        for (var n = node; n; n = n.parent) {
            if (n === source) return null
        }
        return node
    },

    getParent(node) {
        return this.nodesDict[node.data.parent.nameid]
    },

    // Node to focus for a navigation key: undefined if the key is not handled,
    // null if handled but there is nowhere to go.
    keyTarget(key) {
        var f = this.focusedNode;
        switch (key) {
            case "ArrowLeft":
            case "ArrowRight":
                var siblings = f.parent ? f.parent.children : [];
                if (siblings.length < 2) return null
                var i = siblings.indexOf(f) + (key === "ArrowRight" ? 1 : -1);
                return siblings[(i + siblings.length) % siblings.length]
            case "ArrowDown":
            case "Enter":
                return (f.children || [])[0] || null
            case "ArrowUp":
            case "Escape":
            case "Backspace":
                return f.parent || null
            case "Home":
                return f === this.rootNode ? null : this.rootNode
            default:
                return undefined
        }
    },

    // Get node position and properties
    addNodeCtx(node) {
        var zoomCtx = this.zoomCtx;
        var centerX = ((node.x - zoomCtx.centerX) * zoomCtx.scale) + this.centerX;
        var centerY = ((node.y - zoomCtx.centerY) * zoomCtx.scale) + this.centerY + this.nodeOffsetY;
        var rayon = this.nodeRayon(node) * zoomCtx.scale;
        node.ctx = { centerX, centerY, rayon };
        return
    },

    // check geometrical condition
    // p: the mouse pointer
    // n: node pr element currently hovering
    // cond: what to test
    checkIf(p, cond, n) {
        var test = false;
        switch (cond) {
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
                if (!n?.ctx || !this.$tooltip || this.$tooltip.inert || this.$tooltip.classList.contains("is-invisible")) break
                var r = this.$canvas.getBoundingClientRect();
                var rect = this.$tooltip.getBoundingClientRect();
                if (!rect.width || !rect.height) break
                var x1 = rect.left - r.left - 6;
                var x2 = x1 + rect.width + 12;
                var y1 = rect.top - r.top - 6;
                var y2 = y1 + rect.height + 12;
                var x = { x: p.mouseX, y: p.mouseY };
                var a = { x: n.ctx.centerX, y: n.ctx.centerY };
                var y = y2 < a.y ? y2 : y1 > a.y ? y1 : a.y;
                // Keep the visible tooltip and its connecting gap reachable, without masking child nodes.
                test = (x.x >= x1 && x.x <= x2 && x.y >= y1 && x.y <= y2)
                    || (y !== a.y && (x.x - a.x) ** 2 + (x.y - a.y) ** 2 >= n.ctx.rayon ** 2
                        && ptInTriangle(x, a, { x: x1, y }, { x: x2, y }));
                break
            case 'InFocus':
                var x = p.mouseX - this.focusedNode.ctx.centerX;
                var y = p.mouseY - this.focusedNode.ctx.centerY;
                test = x ** 2 + y ** 2 <= (this.focusedNode.ctx.rayon) ** 2;
                break
            case 'InZoomed':
                var x = p.mouseX - this.zoomedNode.ctx.centerX;
                var y = p.mouseY - this.zoomedNode.ctx.centerY;
                test = x ** 2 + y ** 2 <= (this.zoomedNode.ctx.rayon) ** 2;
                break
            case 'InVoid':
                if (!this.rootNode.ctx) break
                var x = p.mouseX - this.rootNode.ctx.centerX;
                var y = p.mouseY - this.rootNode.ctx.centerY;
                // Security margin when movin to a tooltip that is outside the anchor
                test = x ** 2 + y ** 2 > (this.rootNode.ctx.rayon + 10) ** 2;
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
        if (this.isZooming || !node) return
        this.clearNodeTooltip();
        this.clearNodeHover();
        this.app.ports.nodeClickedFromJs.send(node.data.nameid);
    },

    nodeHoveredFromJs(node) {
        if (!this.app || this.isFrozen) return

        var nid;
        if (!node) {
            nid = ""
        } else {
            nid = node.data.nameid;
        }

        this.app.ports.nodeHoveredFromJs.send(nid);
    },

    nodeFocusedFromJs(node) {
        // @DEBUG: why / where would node be undefined ?
        if (!node || !this.app) return
        this.app.ports.nodeFocusedFromJs.send([node.data.nameid, this.gStats.maxdepth]);
    },

    sendNodeLeftClickFromJs(node) {
        if (this.isZooming || !node) return
        this.app.ports.nodeLeftClickedFromJs.send(node.data.nameid);
    },

    sendNodeRightClickFromJs(node) {
        if (this.isZooming || !node) return
        this.app.ports.nodeRightClickedFromJs.send(node.data.nameid);
    },

    sendNodeDraggedFromJs(source, target) {
        if (this.isZooming || !source || !target) return
        this.app.ports.nodeDraggedFromJs.send([source.data.nameid, target.data.nameid]);
    },

    //
    // Init
    //

    // Resize of the canvas by dragging the grips (bottom: height, right: column width, corner: both)
    initResizer() {
        this.bindResizer(document.getElementById('canvasResizer'), false, true);
        this.bindResizer(document.getElementById('canvasResizerV'), true, false);
        this.bindResizer(document.getElementById('canvasResizerC'), true, true);
        // The columns are re-created by Elm on page change, re-apply the user width
        if (this.userColWidth) {
            this.setColWidth(this.userColWidth);
            this.computeGeometry();
            this.sizeDom();
        }
    },

    // Split the row between the canvas column and its sibling, keeping their total width
    setColWidth(w) {
        var $col = this.$canvasParent.parentElement;
        var $colRight = this.$nextToChart.parentElement;
        var wTotal = $col.offsetWidth + $colRight.offsetWidth;
        this.userColWidth = Math.min(Math.max(w, this.minWidth), wTotal - this.minWidth);
        $col.style.flex = "none";
        $col.style.width = this.userColWidth + "px";
        $colRight.style.flex = "none";
        $colRight.style.width = (wTotal - this.userColWidth) + "px";
    },

    bindResizer($h, doX, doY) {
        if (!$h) return

        this.resizers = [...(this.resizers || []), $h];
        $h.onpointerdown = e => {
            if (e.button !== 0 || !this.isActive()) return
            e.preventDefault();
            var x0 = e.clientX, y0 = e.clientY;
            var w0 = this.$canvasParent.parentElement.offsetWidth, h0 = this.height;
            $h.setPointerCapture(e.pointerId);

            $h.onpointermove = ev => {
                if (doX) {
                    this.setColWidth(w0 + ev.clientX - x0);
                }
                if (doY) {
                    this.userHeight = Math.max(this.minHeight, h0 + ev.clientY - y0);
                }

                this.computeGeometry();
                this.sizeDom();
                this.clearNodeTooltip();
                this.clearCanvasCards();
                this.drawCanvas();
            };

            $h.onpointerup = $h.onpointercancel = () => {
                $h.onpointermove = $h.onpointerup = $h.onpointercancel = null;
                this.resizeMe();
            };
        };
    },

    resizeMe() {
        clearTimeout(this.resizeTimer);
        this.resizeTimer = null;
        if (!this.isActive() || !this.graph) return
        this.clearNodeTooltip();
        this.computeGeometry();
        this.sizeDom();
        this.drawCanvas();
        this.drawButtons();
        this.drawNodeHover(this.focusedNode, true);
    },

    isActive() {
        return this.$canvas && this.$canvas.isConnected && document.getElementById(this.canvasId) === this.$canvas;
    },

    dispose() {
        this.cancelMotion();
        clearTimeout(this.initTimer);
        clearTimeout(this.resizeTimer);
        clearTimeout(this.buttonsTimer);
        this.initTimer = this.resizeTimer = this.buttonsTimer = this.pendingInit = null;
        this.clearNodeTooltip();
        if (this.observer) this.observer.disconnect();
        this.observer = null;
        this.handlers.forEach(([element, event, handler]) => element.removeEventListener(event, handler));
        this.handlers = [];
        (this.resizers || []).forEach(el => {
            el.onpointerdown = el.onpointermove = el.onpointerup = el.onpointercancel = null;
        });
        this.resizers = [];
        this.pressed = this.isDragging = false;
        this.dragCandidate = this.dragTarget = this.hoveredNode = null;
        this.clearContextMenu();
        this.nodes = this.nodesDict = this.graph = this.dataNodes = this.gPack = this.gStats = null;
        this.focusedNode = this.zoomedNode = this.rootNode = null;
        this.viewport = null;
        this.exitingNodes = [];
        this.$canvas = this.$canvasParent = this.$nextToChart = null;
        this.$tooltip = this.$canvasButtons = this.ctx2d = null;
    },

    init_canvas() {
        if (this.pendingInit || (this.isActive() && this.graph)) return
        this.dispose();
        this.computeCircleColorRange();
        this.$canvas = document.getElementById(this.canvasId);
        this.$canvasParent = document.getElementById(this.canvasParentId);
        this.$nextToChart = document.getElementById('nextToChart')
        if (!this.$canvas) return

        this.$canvas.classList.remove("is-invisible");
        this.$nextToChart.style.display = "flex";
        this.$nextToChart.style.flexDirection = "column";
        //this.$nextToChart.style.overflowY = "auto";

        this.computeGeometry();
        this.sizeDom();
        this.drawStargate(0, 1);
    },

    drawStargate(radius, down) {
        var canvas = this.$canvas;
        var ctx = canvas.getContext("2d");

        this.clearCanvas(ctx);
        if (radius > 33) {
            down = -1
        } else if (radius <= 0) {
            down = 1
        }


        // First
        var x = this.width / 2;
        var y = this.height / 2;
        var r = this.height / 2.1;

        ctx.lineWidth = 5;
        ctx.strokeStyle = this.hoverCircleColor;
        ctx.shadowOffsetX = 0;
        ctx.shadowOffsetY = 0;
        ctx.shadowBlur = 3;
        ctx.shadowColor = '#656565';
        //ctx.fillStyle = this.colorCircle(0);
        ctx.fillStyle = shadeColor(this.colorCircle(0), -radius) + "55";

        ctx.beginPath();
        ctx.arc(x, y, r, 0, 2 * Math.PI, false);
        //ctx.stroke();
        ctx.fill();
    },

    // Init the canvas and draw the graph
    init(app, data, isInit) {
        var dataNodes = data.data;
        this.app = app;
        if (this.isActive() && this.graph && dataNodes.some(n => n.nameid === this.rootNode.data.nameid)) {
            if (JSON.stringify(dataNodes) !== JSON.stringify(this.dataNodes))
                this.resetGraphPack(dataNodes, data.focusid, data.nodeRenames);
            else this.zoomToNode(data.focusid);
            return !!this.graph
        }
        this.dispose();

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
        if (!this.$canvas) return
        this.$canvas.classList.remove("is-invisible");
        this.ctx2d = this.$canvas.getContext("2d");
        //this.ctx2d.clearRect(0, 0, this.width, this.height);

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

        // Setup nodeTooltip Tooltip
        this.$tooltip = document.getElementById('nodeTooltip');
        this.clearNodeTooltip()

        this.sizeDom();
        this.initResizer();

        //
        // Create Circle Packing - GraphPack
        //

        this.nodeSize = this.nodeSizeTopDown;
        this.resetGraphPack(dataNodes, data.focusid);

        /*////////////////////////////////////////////////////////////
        ////////////////// Events Handler callback ///////////////////
        ////////////////////////////////////////////////////////////*/

        // Listen for clicks on the main canvas
        var nodeClickEvent = e => {
            if (this.isZooming) return false

            if (e.button === 0) {
                // Left click
                var p = this.getPointerCtx(e);
                if (!this.checkIf(p, "InZoomed")) {
                    // Go to parent
                    this.nodeClickedFromJs(this.focusedNode.parent);
                    return
                }

                var node = this.getNodeUnderPointer(e, p);
                var isUpdated = false;
                if (node) {
                    // A node has been clicked
                    isUpdated = true;
                    if (node === this.focusedNode && node === this.rootNode) {
                        // ignore click on the anchor node when focused on it
                        isUpdated = false;
                    }
                }

                if (isUpdated) {
                    if (node.depth > this.focusedNode.depth) {
                        // Goes down
                        // --
                        // Do not dive more that one level down.
                        while (node.parent && (node.parent !== this.focusedNode)) {
                            node = node.parent;
                        };
                    } else if (node.depth < this.focusedNode.depth || node.data.nameid === this.focusedNode.data.nameid) {
                        // Goes up
                        // --
                        // go to the parent node
                        node = this.focusedNode.parent;
                    } else if (node.depth === this.focusedNode.depth) {
                        // Equal depth outside
                        // go to the parent node
                        if (node.parent !== this.focusedNode.parent) {
                            // go to the parent node
                            node = this.focusedNode.parent;
                        } else {
                            // Navigate through same depth node
                        }
                    } else {
                        console.log("click-node-event: this case should not happend, check me")
                    }

                    this.nodeClickedFromJs(node);
                }

            } else if (e.button === 2) {
                // Left click
                //this.sendNodeRightClickFromJs(this.hoveredNode);
            }

            return false;
        };

        // Start a potential node drag (nav/zoom happens on mouseup instead)
        var canvasMouseDownEvent = e => {
            if (e.button !== 0 || this.isZooming || this.isFrozen) return false
            this.pressed = true;
            var p = this.getPointerCtx(e);
            if (!this.checkIf(p, "InZoomed")) return false
            var node = this.getNodeUnderPointer(e, p);
            if (!node || node === this.rootNode || node === this.focusedNode) return false
            this.dragCandidate = { node: node, x: e.clientX, y: e.clientY };
            return false
        };

        // Release: either a drop (move the node) or a plain click (navigate)
        var canvasMouseUpEvent = e => {
            // Ignore a release whose press did not start on the canvas (e.g. from the tooltip)
            if (this.isZooming || e.button !== 0 || !this.pressed) return false
            if (!this.isDragging) {
                this.endDrag();
                return nodeClickEvent(e)
            }
            var source = this.dragCandidate.node;
            var target = this.dragTarget;
            this.endDrag();
            if (target) this.sendNodeDraggedFromJs(source, target);
            return false
        };

        // Cancel a drag released outside the canvas
        var documentMouseUpEvent = e => {
            if (this.pressed) this.endDrag();
            return false
        };

        // Arm the drag past the threshold, then track the drop target
        var dragMoveEvent = e => {
            if (!this.isDragging) {
                var d = this.dragCandidate;
                if (Math.abs(e.clientX - d.x) < this.dragThreshold && Math.abs(e.clientY - d.y) < this.dragThreshold) return false
                this.isDragging = true;
                this.clearNodeTooltip();
                this.$canvas.style.cursor = "grabbing";
            }
            var p = this.getPointerCtx(e);
            this.dragTarget = this.getDropTarget(e, p);
            this.drawDragFeedback(p);
            return false
        };

        // Listen for mouse moves/hoovering on the main canvas
        var canvasMouseMoveEvent = e => {
            if (this.isZooming) return false
            if (this.dragCandidate) return dragMoveEvent(e)
            if (this.isFrozen) return false
            var p = this.getPointerCtx(e);
            var node = this.getNodeUnderPointer(e, p);

            if (node === this.hoveredNode || this.checkIf(p, "InTooltip", this.hoveredNode)) return false
            if (node && this.checkIf(p, "InZoomed"))
                this.drawNodeHover(node, true);
            else if (this.hoveredNode !== this.focusedNode && (node || this.checkIf(p, "InVoid")))
                this.drawNodeHover(this.focusedNode, true);

            return false
        };

        // Entry uses the same target and handoff rules as movement, without arming a drag.
        var canvasMouseEnterEvent = e => {
            if (this.dragCandidate) return false
            return canvasMouseMoveEvent(e)
        };

        // Keep the target while crossing into the tooltip, including outside the canvas bounds.
        var canvasMouseLeaveEvent = e => {
            if (this.isZooming || this.dragCandidate || this.isFrozen) return false
            if (this.$tooltip.contains(e.relatedTarget)) return false
            var p = this.getPointerCtx(e);
            if (!this.checkIf(p, "InCanvas") && !this.checkIf(p, "InTooltip", this.hoveredNode) && this.hoveredNode !== this.focusedNode)
                this.drawNodeHover(this.focusedNode, true);
            return false
        };

        // Catch right click context menu (canvas and node tooltip)
        var contextMenuEvent = e => {
            if (this.isZooming || this.tooltipFrame !== null || (this.$tooltip.contains(e.target) && this.$tooltip.inert)) {
                e.preventDefault();
                return false
            }
            if (!this.isFrozen) {
                e.preventDefault();
                // Touch long-press: drop the pending press so the trailing pointerup is not a click
                this.endDrag();
                this.sendNodeRightClickFromJs(this.hoveredNode);
                this.isFrozen = true;
                return false
            } else {
                // does not work well
                e.preventDefault();
                this.isFrozen = false;
                // Simulate on click to close the action panel
                var $c = document.getElementById(this.$tooltip.dataset.eventAction).querySelector(".clickMe");
                if ($c) {
                    $c.click();
                }
                return true
            }
        };

        // Keyboard navigation (canvas is focusable through tabindex)
        var canvasKeyDownEvent = e => {
            if (e.altKey || e.ctrlKey || e.metaKey) return false
            if (this.isZooming) {
                if (this.keyTarget(e.key) !== undefined) e.preventDefault();
                return false
            }
            if (this.isFrozen) return false
            var node = this.keyTarget(e.key);
            if (node === undefined) return false
            e.preventDefault();
            if (node) this.nodeClickedFromJs(node);
            return false
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
        var isInCanvasButtons = e => {
            var p = this.getPointerCtx(e);
            var isInButtons = false;
            this.$canvasButtons.childNodes.forEach(o => {
                isInButtons |= this.checkIf(p, 'InButtons', o);
            });
            return isInButtons
        };
        var canvasButtonsDown = e => isInCanvasButtons(e) ? true : canvasMouseDownEvent(e);
        var canvasButtonsUp = e => isInCanvasButtons(e) ? true : canvasMouseUpEvent(e);
        var canvasButtonsMove = e => isInCanvasButtons(e) ? true : canvasMouseMoveEvent(e);

        // Tooltip Clicks
        var tooltipTensionClick = e => {
            if (this.isZooming || this.$tooltip.inert || e.button !== 0) return true
            // A click on the tooltip closes an open menu instead of opening a tension.
            if (this.isFrozen) {
                this.isFrozen = false;
                return false
            }
            this.sendNodeLeftClickFromJs(this.hoveredNode);
            return true
        };
        var tooltipActionClick = e => {
            if (this.isZooming || this.$tooltip.inert || e.button !== 0) return true
            if (e.target.closest(".clickMe")) this.isFrozen = true;
            return true
        };

        // On Resize handle
        var resizeEvent = () => {
            clearTimeout(this.resizeTimer);
            this.resizeTimer = setTimeout(() => this.resizeMe(), this.delta);
        };

        //////////////////////////////////////////////////////////////
        /////////////////////// Initiate /////////////////////////////
        //////////////////////////////////////////////////////////////

        if (!this.graph) return
        console.log("Orga Canvas Initalization");
        this.isFrozen = false;
        this.endDrag();

        // Prime node.ctx (canvas positions) so hover/focus drawing works before the first zoom.
        this.nodes.forEach(n => this.addNodeCtx(n));

        // Keyboard navigation ready on load, unless the user is already in a field
        if (!document.activeElement || document.activeElement === document.body)
            this.$canvas.focus({ preventScroll: true });

        //
        // Event listeners
        //

        var $subTooltipTension = document.getElementById(this.$tooltip.dataset.eventTension);
        var $subTooltipAction = document.getElementById(this.$tooltip.dataset.eventAction);

        // Cleanup old handlers to avoid dragons !
        for (var i = 0; i < this.handlers.length; i++) {
            this.handlers[i][0].removeEventListener(this.handlers[i][1], this.handlers[i][2]);
        }

        this.handlers = [
            // Canvas pointer events (mouse, touch and pen alike)
            [this.$canvas, "pointermove", canvasMouseMoveEvent],
            [this.$canvas, "pointerenter", canvasMouseEnterEvent],
            [this.$canvas, "pointerleave", canvasMouseLeaveEvent],
            [this.$canvas, "pointerdown", canvasMouseDownEvent],
            [this.$canvas, "pointerup", canvasMouseUpEvent],
            [this.$canvas, "pointercancel", documentMouseUpEvent],
            [this.$canvas, "contextmenu", contextMenuEvent],
            [this.$canvas, "keydown", canvasKeyDownEvent],
            //[this.$canvas, "wheel", contextMenuEvent], // or "scroll" ?
            [document, "pointerup", documentMouseUpEvent],
            [window, "resize", resizeEvent],
            // Canvas buttons events
            [this.$canvasButtons, "pointerdown", canvasButtonsDown],
            [this.$canvasButtons, "pointerup", canvasButtonsUp],
            [this.$canvasButtons, "pointermove", canvasButtonsMove],
            // Tooltip events
            [$subTooltipTension, "mousedown", tooltipTensionClick],
            [$subTooltipAction, "mousedown", tooltipActionClick],
            [this.$tooltip, "contextmenu", contextMenuEvent],
        ];

        // Setup handlers
        for (var i = 0; i < this.handlers.length; i++) {
            this.handlers[i][0].addEventListener(this.handlers[i][1], this.handlers[i][2]);
        }
        this.observer = new MutationObserver(() => {
            if (!this.isActive()) this.dispose();
        });
        this.observer.observe(document.body, { childList: true, subtree: true });

        //
        // ELM Subscriptions
        //

        if (isInit) {

            // ToggleGrahReverse button
            app.ports.sendToggleGraphReverse.subscribe(e => {
                if (!this.isActive() || !this.graph) return
                if (this.nodeSize === this.nodeSizeTopDown) {
                    this.nodeSize = this.nodeSizeBottomUp;
                } else {
                    this.nodeSize = this.nodeSizeTopDown;
                }

                this.resetGraphPack(this.dataNodes, this.focusedNode.data.nameid);
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
