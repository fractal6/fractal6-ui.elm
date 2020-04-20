// @Debug use import with a bundler/webpack !

const setpixelated = (ctx, v) => {
    ctx['imageSmoothingEnabled'] = v;       /* standard */
    ctx['oImageSmoothingEnabled'] = v;      /* Opera */
    ctx['webkitImageSmoothingEnabled'] = v; /* Safari */
    ctx['msImageSmoothingEnabled'] = v;     /* IE */
    //ctx['mozImageSmoothingEnabled'] = v;    /* Firefox (deprecated) */
}

// Flat list of nodes (unordered) to nested tree structure
// from: https://stackoverflow.com/questions/18017869/build-tree-array-from-flat-array-in-javascript/40732240#40732240
const formatGraph = dataset =>  {
    var dataTree = [];
    var hashTable = Object.create(null);
    dataset.forEach( aData => hashTable[aData.ID] = {
        ...aData,
        children : [],
        depth : 0
    })
    dataset.forEach( aData => {
        if(aData.parentID) {
            hashTable[aData.parentID].children.push(hashTable[aData.ID])
        } else {
            dataTree.push(hashTable[aData.ID])
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
        obj.children.forEach(function (d) {
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

// Main drawing function
function drawAll(app, graph) {
    /*////////////////////////////////////////////////////////////
    //////////////// Style Constants  ////////////////////////////
    ////////////////////////////////////////////////////////////*/

    //var colorDarker1 = "#303030";
    //var colorDarker2 = "#313131";
    //var colorDarker1 = "#DFE1E2";
    //var colorDarker2 = "#DFE1E2";
    var colorDarker1 = "#EDFCFF";
    var colorDarker2 = "#EDFCFF";

    //var backgroundColor = "#404040";  // darker
    //var backgroundColor = "#DDE6F9";  // lighter
    //var backgroundColor = window.getComputedStyle(document.getElementById("body"), null).getPropertyValue("background-color");
    var backgroundColor = colorDarker1;

    var colorCircleRange = ['#bfbfbf','#838383','#4c4c4c','#1c1c1c', '#000000'];
    //var colorCircleRange = ['#d9d9d9','#838383','#4c4c4c','#1c1c1c', '#000000'];

    var canvasParentId = "canvasParent";
    var canvasId = "canvasOrga";
    var hiddenCanvasId = "hiddenCanvasOrga";
    var leafColor = "white";
    var minZoomDuration = 1250, // 1500
        zoomFactorCircle = 2.05,
        zoomFactorRole = 2.2;

    var hoverCircleColor =  "black",
        hoverCircleWidth = 1.5; // waring, can break stroke with canvas drawing.

    /*////////////////////////////////////////////////////////////
    ////////////////// Create Set-up variables  //////////////////
    ////////////////////////////////////////////////////////////*/

    // Get the chart div
    var $canvasParent = document.getElementById(canvasParentId);

    // Add the tooltip
    //var $tooltip = document.getElementById('nodeTooltip');
    var $tooltip = document.createElement('div');
    document.body.appendChild($tooltip);
    $tooltip.setAttribute('id', 'nodeTooltip');

    // @FIX: put all global variables inside that (shorter name?) !
    var globalCtx = {
        minWidth : 400,
        minHeight : 400,
    }
    var computedWidth = $canvasParent.offsetWidth; //var computedWidth = parseInt(window.getComputedStyle($canvasParent).width, 10);
    var computedHeight = (window.innerHeight)/2;

    globalCtx.width = Math.max(computedWidth, globalCtx.minWidth);
    globalCtx.height = Math.max(computedHeight, globalCtx.minHeight); //(computedHeight > computedWidth ?  computedWidth: computedHeight );
    globalCtx.centerX = globalCtx.width/2;
    globalCtx.centerY = globalCtx.height/2;
    globalCtx.mobileSize = (window.innerWidth < 768 ? true : false);

    /*////////////////////////////////////////////////////////////
    ////////// Create and Bind Canvas to the DOM  ////////////////
    ////////////////////////////////////////////////////////////*/

    // Create the visible canvas and context
    var canvas = d3.select("#"+canvasParentId).append("canvas")
        .attr("id", canvasId)
        .attr("width", globalCtx.width)
        .attr("height", globalCtx.height);
    var context = canvas.node().getContext("2d");
    context.clearRect(0, 0, globalCtx.width, globalCtx.height);
    setpixelated(context, true); // @debug: do we need this ?

    // Create a hidden canvas in which each circle will have a different color.
    // We use this to capture the clicked on circle
    var hiddenCanvas = d3.select("#"+canvasParentId).append("canvas")
        .attr("id", hiddenCanvasId)
        .attr("width", globalCtx.width)
        .attr("height", globalCtx.height)
        .style("display","none");
    var hiddenContext = hiddenCanvas.node().getContext("2d");
    hiddenContext.clearRect(0, 0, globalCtx.width, globalCtx.height);

    var $canvas = document.getElementById(canvasId);
    var $hidden_canvas = document.getElementById(hiddenCanvasId);

    //
    // Update Html Elemens
    //

    // Set height of parent sibling
    var $nextToChart = document.getElementById('nextToChart')
    $nextToChart.style.minHeight = 2*globalCtx.height+"px";
    $nextToChart.style.display = "flex";
    $nextToChart.style.flexDirection = "column";
    //$nextToChart.style.overflowY = "auto";

    // Set Canvas button
    var $canvasButtons = document.getElementById('canvasButtons');
    $canvasButtons.style.top = "-"+ globalCtx.height+"px";
    $canvasButtons.classList.remove("is-hidden");

    /*////////////////////////////////////////////////////////////
    //////////// Create Circle Scales and Propertie /////////////
    ////////////////////////////////////////////////////////////*/
    var circlesPadding = 1.8;
    var diameter = Math.min(globalCtx.width*0.97, globalCtx.height*0.97),
        radius = diameter / 2;

    var zoomInfo = {
        centerX: globalCtx.centerX,
        centerY: globalCtx.centerY,
        scale: 1
    };

    // Mapping function from a node depth to color.
    const colorCircle = d3.scaleOrdinal()
        .domain(Array.from({length:colorCircleRange.length},(v,k)=>k))
        .range(colorCircleRange)
        .unknown(backgroundColor);

    // Determine the node size in the circle packing
    // Returns: int f(n.depth, n.neigbor, n.cumchild)
    const nodeSizeTopDown = (n, stats) => {
        var size = 10000/(stats.maxdepth)**(Math.max(1.5, n.depth))
        return size
    }
    const nodeSizeBottomUp = (n, stats) => {
        var sizeDefault = 4;
        var size = 10000/(stats.maxdepth)**(Math.max(0, sizeDefault - n.depth))
        return size
    }
    var nodeSize = nodeSizeTopDown;

    // Determine the node order in the circle packing
    const nodeOrder = (n1, n2) => {
        // n1.createdAt < n2.createdAt // node order
        return 0
    }

    /*////////////////////////////////////////////////////////////
    ////////////////// Create Circle Packing /////////////////////
    ////////////////////////////////////////////////////////////*/

    var gStats; // Receive graph global statistics
    var cPack; // Receive D3 data structure
    var nodes; // List of d3 nodes
    var rootNode; // The root node of the graph
    var focusedNode; // The node that has the active focus
    var hoveredNode; // The node that is curently hoovered
    // Dataset to swich between color of a circle (in the hidden canvas) and the node data
    var colToCircle ;

    graph = formatGraph(graph);
    if (graph.length > 1) console.warn("More than 1 graph given -> Some nodes are not connected.")
    graph = graph[0]
    gStats = computeDepth(graph);
    //console.log(graph);

    cPack = d3.pack()
        .padding(circlesPadding)
        .size([diameter, diameter])
    (d3.hierarchy(graph)
        .sum(d => nodeSize(d, gStats))
        .sort(nodeOrder));

    nodes = cPack.descendants(graph);
    rootNode = nodes[0];
    // @DEBUG: Reset globalCtx
    var colToCircle = {};
    focusedNode = rootNode;
    hoveredNode = null;

    // @Ddebug global context
    var cWidth = canvas.attr("width");
    var cHeight = canvas.attr("height");
    var nodeCount = nodes.length;

    var backgoundGrd = context.createLinearGradient(0, 0, cWidth, 0);
    backgoundGrd.addColorStop(0, colorDarker1);
    backgoundGrd.addColorStop(1, colorDarker2);

    /*////////////////////////////////////////////////////////
    ///////////////// Helpers function ///////////////////////
    ////////////////////////////////////////////////////////*/

    //Generates the next color in the sequence, going from 0,0,0 to 255,255,255.
    //From: https://bocoup.com/weblog/2d-picking-in-canvas
    var nextCol = 1; // how to use this/interface ?
    function genColor() {
        var ret = [];
        // via http://stackoverflow.com/a/15804183
        if(nextCol < 16777215) {
            ret.push(nextCol & 0xff); // R
            ret.push((nextCol & 0xff00) >> 8); // G
            ret.push((nextCol & 0xff0000) >> 16); // B

            nextCol += 100; // This is exagerated for this example and would ordinarily be 1.
        }
        var col = "rgb(" + ret.join(',') + ")";
        return col;
    }

    // Get the mouse coordinate whithin the canvas reference.
    function getPointerCtx(e) {
        var r = $canvas.getBoundingClientRect();
        var mouseX = (e.clientX - r.left);
        var mouseY = (e.clientY - r.top);
        return {mouseX, mouseY}
    }

    // Returns the path from root to node.
    function getNodePath(node) {
        var path = cPack.path(node).map(n => {
            return {
                name: n.data.name,
                nidjs: n.color,
            };
        });
        return path
    }

    // Get node position and properties
    function addNodeCtx(node) {
        var ctx,
            centerX, centerY, rayon;

        centerX = ((node.x - zoomInfo.centerX) * zoomInfo.scale) + globalCtx.centerX;
        centerY = ((node.y - zoomInfo.centerY) * zoomInfo.scale) + globalCtx.centerY;
        if (node.data.type_ === "Role") {
            rayon = node.r * 0.95 ;
        } else {
            rayon = node.r;
        }
        rayon *= zoomInfo.scale;
        node.ctx = {centerX, centerY, rayon};
        return
    }

    function getNodeUnderPointer(e) {
        //Figure out where the mouse click occurred.
        var p = getPointerCtx(e);

        // Get the corresponding pixel color on the hidden canvas and look up the node in our map.
        // This will return that pixel's color
        var col = hiddenContext.getImageData(p.mouseX, p.mouseY, 1, 1).data;
        //Our map uses these rgb strings as keys to nodes.
        var colString = "rgb(" + col[0] + "," + col[1] + ","+ col[2] + ")";
        var node = colToCircle[colString];
        if (node) {
            addNodeCtx(node);
        }
        return node;
    }

    // Draw node border
    function drawNodeHover(ctx, node) {
        if (hoveredNode) clearNodeHover(ctx, hoveredNode);

        // Draw border
        ctx.beginPath();
        ctx.arc(node.ctx.centerX, node.ctx.centerY,
            node.ctx.rayon+1, 0, 2 * Math.PI, true);
        ctx.lineWidth = hoverCircleWidth;
        ctx.strokeStyle = hoverCircleColor;
        ctx.stroke();

        // Draw tooltip
        drawNodeTooltip(node);

        // Update global context
        node.isHovered = true;
        hoveredNode = node; //@debug: use globalCtx
        return
    }

    // Clean node hovering
    function clearNodeHover(ctx, node) {
        // Clear node Border
        ctx.beginPath();
        ctx.arc(node.ctx.centerX, node.ctx.centerY,
            node.ctx.rayon+1, 0, 2 * Math.PI, true);
        ctx.lineWidth = 3;
        ctx.strokeStyle = colorCircle(node.depth-1);
        ctx.stroke();

        // Clear node tooltip
        clearNodeTooltip();

        // Update context
        node.isHovered = false;
        hoveredNode = null; //@debug: use globalCtx
        return
    }

    function drawNodeTooltip(node) {
        var r = $canvas.getBoundingClientRect();
        // == add tooltip
        // @DEBUG: tooltip neeed to be displayed to get its clientWidth.
        $tooltip.textContent = node.data.name;
        $tooltip.style.display = "block";
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
            clearNodeTooltip();
        } else {
            $tooltip.style.left = l + "px";
            $tooltip.style.top = t + "px";
        }
        return
    }

    // Clear node tooltip.
    function clearNodeTooltip() {
        $tooltip.style.display = "none";
        return
    }

    // check geometrical condition
    function checkIf(p, cond, nodeOrElt) {
        var test;
        switch(cond) {
            case 'InCanvas':
                var r = $canvas.getBoundingClientRect();
                var x2 = r.width;
                var y2 = r.height;
                test = (p.mouseX > 0) && (p.mouseY > 0) && (p.mouseX < x2) && (p.mouseY < y2);
                break
            case "InButtons":
                var $btn = nodeOrElt;
                var r = $canvas.getBoundingClientRect();
                var rBtn = $btn.getBoundingClientRect();
                var x1 = rBtn.left - r.left;
                var y1 = rBtn.top - r.top;
                var x2 = x1 + rBtn.width;
                var y2 = y1 + rBtn.height;
                test = (p.mouseX > x1) && (p.mouseY > y1) && (p.mouseX < x2) && (p.mouseY < y2);
                break
            case 'InTooltip':
                var n = nodeOrElt;
                var h = $tooltip.clientHeight +12;
                var w = $tooltip.clientWidth/2 +8;
                var x1 = n.ctx.centerX - w;
                var x2 = n.ctx.centerX + w;
                var y1 = n.ctx.centerY - n.ctx.rayon - h;
                var y2 = n.ctx.centerY - n.ctx.rayon*0.6;
                test = (p.mouseX > x1) && (p.mouseX < x2) && (p.mouseY > y1) && (p.mouseY < y2);
                break
            default:
                console.error("Unknown condition: %s", cond)
        }
        //console.log(cond, "?", test);
        return test
    }

    //////////////////////////////////////////////////////////////
    ///////////////// Canvas draw function ///////////////////////
    //////////////////////////////////////////////////////////////

    //Clear canvas
    function clearCanvas(ctx) {
        ctx.fillStyle = backgoundGrd;
        ctx.rect(0,0,cWidth,cHeight);
        ctx.fill();
    }

    //The draw function of the canvas that gets called on each frame
    function drawCanvas(ctx, hidden) {
        clearCanvas(ctx);

        //Select our dummy nodes and draw the data to canvas.
        var node,
            _name , type_ ,
            circleColor;
        // It's slightly faster than nodes.forEach()
        for (var i = 0; i < nodeCount; i++) {
            node = nodes[i];
            _name = node.data.name;
            type_ = node.data.type_;

            if (type_ === "Hidden") {
                continue;
            } else {
                addNodeCtx(node);
            }

            //If the hidden canvas was send into this function and it does not yet have a color,
            //generate a unique one.
            if(hidden) {
                if(node.color === undefined) {
                    // If we have never drawn the node to the hidden canvas get a new color for it and put it in the dictionary.
                    node.color = genColor();
                    colToCircle[node.color] = node;
                }
                // On the hidden canvas each rectangle gets a unique color.
                circleColor = node.color;
            } else {
                circleColor = (type_ === "Circle") ? colorCircle(node.depth) : leafColor;
            }

            //Draw each circle
            ctx.beginPath();
            ctx.fillStyle = circleColor;
            ctx.arc(node.ctx.centerX, node.ctx.centerY, node.ctx.rayon,
                0, 2 * Math.PI, true);
            ctx.fill();

            if (!hidden) {
                if (node.isHovered) {
                    ctx.lineWidth = hoverCircleWidth;
                    ctx.strokeStyle = hoverCircleColor;
                    ctx.stroke();
                }

                if (type_ === "Role") {
                    var text = _name.substring(0,3).replace(/./,x=>x.toUpperCase())
                    var font_size = 19;
                    var text_display = false;
                    //for (var ii=0; ii < 2; ii++) {
                    // Search font that fit
                    ctx.font = font_size +"px Arial";
                    if (ctx.measureText(text).width+1 < node.ctx.rayon*2) {
                        text_display = true;
                        //break;
                    } else {
                        font_size--;
                    }
                    //}

                    if (text_display) {
                        ctx.beginPath();
                        ctx.fillStyle = "black";
                        ctx.textAlign = "center";
                        ctx.fillText(text,
                            node.ctx.centerX, node.ctx.centerY+7);
                        //ctx.shadowColor = '#999';
                        //ctx.shadowBlur = 20;
                        //ctx.shadowOffsetX = 5;
                        //ctx.shadowOffsetY = 5;
                        ctx.fill();
                    }
                } else {
                    //if (focusedNode.depth == node.depth || focusedNode.depth == node.depth-1 ) {
                    //    ctx.beginPath();
                    //    ctx.fillStyle = "white";
                    //    ctx.fillCircleText(_name,
                    //        node.ctx.centerX, node.ctx.centerY,
                    //        node.ctx.rayon, -Math.PI*0.7);
                    //    ctx.fill()
                    //}
                }
            }
        }//for i
    }//function drawCanvas

    /*////////////////////////////////////////////////////////////
    /////////////////// Event Handler ////////////////////////////
    ////////////////////////////////////////////////////////////*/

    // Listen for clicks on the main canvas
    var nodeClickEvent = function(e) {
        if (isZooming) {
            return false
        }
        var node = getNodeUnderPointer(e);
        var isUpdated = false;
        if (node) {
            isUpdated = true;
            if (node === focusedNode) {
                // go to the parent node
                if (node !== rootNode) {
                    node = node.parent;
                } else {
                    isUpdated = false;
                }
            }
        }

        if (isUpdated) {
            clearNodeTooltip();
            zoomToNode(node);
        }

        return false;
    }//MouseClickEvent

    // Listen for mouse moves/hooverin on the main canvas
    var canvasMouseMoveEvent = function(e) {
        if (isZooming) {
            return false
        }
        var ctx = context;
        var p = getPointerCtx(e);
        var node = getNodeUnderPointer(e);        // @Warning, it updates ctx attributes.
        var isInTooltip = false;
        if (hoveredNode) {
            isInTooltip = checkIf(p, "InTooltip", hoveredNode);
        }

        if (node) {
            if (node !== hoveredNode && !isInTooltip) {
                drawNodeHover(ctx, node);
            }
        } else if (hoveredNode) {
            var isInCanvas = checkIf(p, "InCanvas", null); // possibliy link to issue #9232dcd
            if (hoveredNode == rootNode) {
                // keep it
            } else if (!isInTooltip && isInCanvas) clearNodeHover(ctx, hoveredNode);
        } else {
            drawNodeHover(ctx, focusedNode);
        }
        return false
    }//event MouseMove

    // Listen for mouse moves/hooverout on the main canvas
    var canvasMouseLeaveEvent = function(e) {
        var ctx = context;
        var p = getPointerCtx(e);
        var isInCanvas = checkIf(p, "InCanvas", null); // purpose of that is possibliy linked to issue #9232dcd
        if (!isInCanvas) drawNodeHover(ctx, focusedNode);

        // Remove hovered except if exiting from an tooltip
        //var isInCanvas = checkIf(p, "InCanvas", null); // purpose of that is possibliy linked to issue #9232dcd
        //if (hoveredNode) {
        //    var isInTooltip = checkIf(p, "InTooltip", hoveredNode);
        //    // Keep the focus when getting out from the canvas
        //    if (!isInTooltip && !isInCanvas) clearNodeHover(ctx, hoveredNode);
        //}

        return false
    }//event MouseLeave

    /*////////////////////////////////////////////////////////////
    ///////////////////// Zoom Function //////////////////////////
    /////////////////////////////////////////////////////////////*/

    //Based on the generous help by Stephan Smola
    //http://bl.ocks.org/smoli/d7e4f9199c15d71258b5

    var isZooming = false;
    var ease = d3.easePolyInOut.exponent(3)
    //var ease = d3.easePoly.exponent(4)
    timeElapsed = 0,
    interpolator = null,
    vOld = [focusedNode.x, focusedNode.y, focusedNode.r * zoomFactorCircle];

    //Create the interpolation function between current view and the clicked on node
    // If `d:duration` is given, it overwrite the zoom duration. Give a low value for flush reset.
    function zoomToNode(focus, d) {
        if (isZooming) {
            return false
        }
        focusedNode = focus; // @DEBUG: global context

        var zoomFactor = zoomFactorCircle;
        if (focusedNode.data.type_ === 'Role') {
            zoomFactor = zoomFactorRole;
        }
        var v = [focusedNode.x, focusedNode.y, focusedNode.r * zoomFactor]; //The center and width of the new "viewport"
        var maxDuration = (d === undefined ? minZoomDuration*2 : d);
        interpolator = d3.interpolateZoom(vOld, v); //Create interpolation between current and new "viewport"
        duration = Math.max(interpolator.duration, minZoomDuration); //Interpolation gives back a suggested duration
        timeElapsed = 0; //Set the time elapsed for the interpolateZoom function to 0
        vOld = v; //Save the "viewport" of the next state as the next "old" state

        var	dt = 0;
        var t = d3.timer(function(elapsed) {
            //stats.begin();
            isZooming = true;
            var finished = interpolateZoom(elapsed - dt);
            dt = elapsed;
            drawCanvas(context);
            //stats.end();
            if (finished || elapsed > maxDuration) {
                isZooming = false;
                // We actually only need to draw the hidden canvas when there is an interaction.
                drawCanvas(hiddenContext, true);
                drawNodeHover(context, focusedNode);
                updateFocusedNodeElm(focusedNode);
                t.stop();
            }
        });

    }//function zoomToNode

    //Perform the interpolation and continuously change the zoomInfo while the "transition" occurs
    function interpolateZoom(dt) {
        if (interpolator) {
            timeElapsed += dt;
            var t = ease(timeElapsed / duration);

            zoomInfo.centerX = interpolator(t)[0];
            zoomInfo.centerY = interpolator(t)[1];
            zoomInfo.scale = diameter / interpolator(t)[2];

            if (timeElapsed >= duration)
            {
                interpolator = null;
                return true;
            } else {
                return false;
            }
        }
    }//function interpolateZoom

    //////////////////////////////////////////////////////////////
    /////////////////////// Initiate /////////////////////////////
    //////////////////////////////////////////////////////////////

    document.getElementById(canvasId).addEventListener("mousemove", canvasMouseMoveEvent);
    document.getElementById(canvasId).addEventListener("mouseleave", canvasMouseLeaveEvent);
    document.getElementById(canvasId).addEventListener("mousedown", nodeClickEvent);
    // Canvas Button event redirection
    document.getElementById("canvasButtons").addEventListener("mousedown", function(e) {
        if (!checkIf(getPointerCtx(e), 'InButtons', document.getElementById('inv_cvbtn'))) {
            return nodeClickEvent(e)
        }
        return true
    });
    document.getElementById("canvasButtons").addEventListener("mousemove", function(e) {
        if (!checkIf(getPointerCtx(e), 'InButtons', document.getElementById('inv_cvbtn'))) {
            return canvasMouseMoveEvent(e)
        }
        return true
    });

    //First zoom to get the circles to the right location
    // then timer the interpolateZoom and rendering
    console.log("Canvas initalization");
    zoomToNode(rootNode, 250); //drawCanvas(context);

    // @DEBUG: Implement redrawCanvas() !
    window.onresize = function () {
        //$canvas.style.width = '100%';
        //$canvas.style.height = canvas.style.height * .75;

        //redrawCanvas(focusedNode);
        //drawAll(app, graph);
        console.log("redrawCanvas not implemented yet !")
    }

    //
    // Elm Ports
    //

    function updateFocusedNodeElm(node) {
        var path = getNodePath(node);
        app.ports.receiveData.send({
            nidjs    : node.color,
            name     : node.data.name,
            nodeType : node.data.type_,
            path     : path
        });
    }

    //
    // ELM Subscriptions
    //

    app.ports.sendNodeFocus.subscribe(function(nid) {
        var node = colToCircle[nid];
        if (hoveredNode) clearNodeHover(context, hoveredNode);
        zoomToNode(node);
    });

    app.ports.sendToggleGraphReverse.subscribe(function(e) {
        if (nodeSize.name == "nodeSizeTopDown") {
            nodeSize = nodeSizeBottomUp;
        } else {
            nodeSize = nodeSizeTopDown;
        }

        if (hoveredNode) clearNodeHover(context, hoveredNode);

        cPack = d3.pack()
            .padding(circlesPadding)
            .size([diameter, diameter])
        (d3.hierarchy(graph)
            .sum(d => nodeSize(d, gStats))
            .sort(nodeOrder));
        nodes = cPack.descendants(graph);

        // @Debug: Reset globalCtx
        rootNode = nodes[0];
        focusedNode = rootNode;
        hoveredNode = null;
        colToCircle = {};

        clearCanvas(context);
        clearCanvas(hiddenContext);
        // @debug reset !
        vOld = [focusedNode.x, focusedNode.y, focusedNode.r * zoomFactorCircle];
        zoomToNode(rootNode, 10);
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


}//drawAll
