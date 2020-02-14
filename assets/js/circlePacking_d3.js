(function(){
    const FILL = 0;        // const to indicate filltext render
    const STROKE = 1;
    var renderType = FILL; // used internal to set fill or stroke text
    const multiplyCurrentTransform = true; // if true Use current transform when rendering
                                           // if false use absolute coordinates which is a little quicker
                                           // after render the currentTransform is restored to default transform



    // measure circle text
    // ctx: canvas context
    // text: string of text to measure
    // r: radius in pixels
    //
    // returns the size metrics of the text
    //
    // width: Pixel width of text
    // angularWidth : angular width of text in radians
    // pixelAngularSize : angular width of a pixel in radians
    var measure = function(ctx, text, radius){
        var textWidth = ctx.measureText(text).width; // get the width of all the text
        return {
            width               : textWidth,
            angularWidth        : (1 / radius) * textWidth,
            pixelAngularSize    : 1 / radius
        };
    }

    // displays text along a circle
    // ctx: canvas context
    // text: string of text to measure
    // x,y: position of circle center
    // r: radius of circle in pixels
    // start: angle in radians to start.
    // [end]: optional. If included text align is ignored and the text is
    //        scaled to fit between start and end;
    // [forward]: optional default true. if true text direction is forwards, if false  direction is backward
    var circleText = function (ctx, text, x, y, radius, start, end, forward) {
        var i, textWidth, pA, pAS, a, aw, wScale, aligned, dir, fontSize;
        if(text.trim() === "" || ctx.globalAlpha === 0){ // dont render empty string or transparent
            return;
        }
        if(isNaN(x) || isNaN(y) || isNaN(radius) || isNaN(start) || (end !== undefined && end !== null && isNaN(end))){ //
            throw TypeError("circle text arguments requires a number for x,y, radius, start, and end.")
        }
        aligned = ctx.textAlign;        // save the current textAlign so that it can be restored at end
        dir = forward ? 1 : forward === false ? -1 : 1;  // set dir if not true or false set forward as true
        pAS = 1 / radius;               // get the angular size of a pixel in radians
        textWidth = ctx.measureText(text).width; // get the width of all the text
        if (end !== undefined && end !== null) { // if end is supplied then fit text between start and end
            pA = ((end - start) / textWidth) * dir;
            wScale = (pA / pAS) * dir;
        } else {                 // if no end is supplied correct start and end for alignment
            // if forward is not given then swap top of circle text to read the correct direction
            if(forward === null || forward === undefined){
                if(((start % (Math.PI * 2)) + Math.PI * 2) % (Math.PI * 2) > Math.PI){
                    dir = -1;
                }
            }
            pA = -pAS * dir ;
            wScale = -1 * dir;
            switch (aligned) {
            case "center":       // if centered move around half width
                start -= (pA * textWidth )/2;
                end = start + pA * textWidth;
                break;
            case "right":// intentionally falls through to case "end"
            case "end":
                end = start;
                start -= pA * textWidth;
                break;
            case "left":  // intentionally falls through to case "start"
            case "start":
                end = start + pA * textWidth;
            }
        }

        ctx.textAlign = "center";                     // align for rendering
        a = start;                                    // set the start angle
        for (var i = 0; i < text.length; i += 1) {    // for each character
            aw = ctx.measureText(text[i]).width * pA; // get the angular width of the text
            var xDx = Math.cos(a + aw / 2);           // get the yAxies vector from the center x,y out
            var xDy = Math.sin(a + aw / 2);
            if(multiplyCurrentTransform){ // transform multiplying current transform
                ctx.save();
                if (xDy < 0) { // is the text upside down. If it is flip it
                    ctx.transform(-xDy * wScale, xDx * wScale, -xDx, -xDy, xDx * radius + x, xDy * radius + y);
                } else {
                    ctx.transform(-xDy * wScale, xDx * wScale, xDx, xDy, xDx * radius + x, xDy * radius + y);
                }
            }else{
                if (xDy < 0) { // is the text upside down. If it is flip it
                    ctx.setTransform(-xDy * wScale, xDx * wScale, -xDx, -xDy, xDx * radius + x, xDy * radius + y);
                } else {
                    ctx.setTransform(-xDy * wScale, xDx * wScale, xDx, xDy, xDx * radius + x, xDy * radius + y);
                }
            }
            if(renderType === FILL){
                ctx.fillText(text[i], 0, 0);    // render the character
            }else{
                ctx.strokeText(text[i], 0, 0);  // render the character
            }
            if(multiplyCurrentTransform){  // restore current transform
                ctx.restore();
            }
            a += aw;                     // step to the next angle
        }
        // all done clean up.
        if(!multiplyCurrentTransform){
            ctx.setTransform(1, 0, 0, 1, 0, 0); // restore the transform
        }
        ctx.textAlign = aligned;            // restore the text alignment
    }
    // define fill text
    var fillCircleText = function(text, x, y, radius, start, end, forward){
        renderType = FILL;
        circleText(this, text, x, y, radius, start, end, forward);
    }
    // define stroke text
    var strokeCircleText = function(text, x, y, radius, start, end, forward){
        renderType = STROKE;
        circleText(this, text, x, y, radius, start, end, forward);
    }
    // define measure text
    var measureCircleTextExt = function(text,radius){
        return measure(this, text, radius);
    }
    // set the prototypes
    CanvasRenderingContext2D.prototype.fillCircleText = fillCircleText;
    CanvasRenderingContext2D.prototype.strokeCircleText = strokeCircleText;
    CanvasRenderingContext2D.prototype.measureCircleText = measureCircleTextExt;
})();


function setpixelated(ctx, v){
    ctx['imageSmoothingEnabled'] = v;       /* standard */
    ctx['mozImageSmoothingEnabled'] = v;    /* Firefox */
    ctx['oImageSmoothingEnabled'] = v;      /* Opera */
    ctx['webkitImageSmoothingEnabled'] = v; /* Safari */
    ctx['msImageSmoothingEnabled'] = v;     /* IE */
}

hackDepth = function (obj, depth, neigbor) {
    // Add a `depth` attribute to  each node
    // Returns: the max depth (the number of depth starting from 0.
    var maxdepth = 0;
    var cumchild = 0

    if (depth === undefined) {
        var currentdepth = 0;
        var neigbor = 1
    } else {
        var currentdepth = depth;
        neigbor = neigbor
    }

    obj.depth = currentdepth;
    obj.neigbor = neigbor

    if (obj.children) {
        obj.children.forEach(function (d) {
            var d = hackDepth(d, currentdepth+1, obj.children.length)
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

    return {maxdepth, cumchild };
}

function drawAll(app, dataset) {

    /*************************
     ****** Constants ********
     ************************/

    var backgroundColor = "#404040";  // darker
    //var backgroundColor = "#272a2f";  // darker
    //var backgroundColor = "#ECECEC";  // lighter
    //var backgroundColor = "#DDE6F9";  // lighter
    //var backgroundColor = window.getComputedStyle(document.getElementById("body"), null).getPropertyValue("background-color");
    //
    var colorDarker1 = "#303030";
    var colorDarker2 = "#313131";
    //var colorDarker1 = "#DFE1E2";
    //var colorDarker2 = "#DFE1E2";
    var colorDarker1 = "#EDFCFF";
    var colorDarker2 = "#EDFCFF";

    var colorCircleRange = ['#bfbfbf','#838383','#4c4c4c','#1c1c1c', '#000000'];
    //var colorCircleRange = ['#d9d9d9','#838383','#4c4c4c','#1c1c1c', '#000000'];

    var canvasParentId = "chart";
    var canvasId = "canvasOrga";
    var hiddenCanvasId = "hiddenCanvasOrga";
    var leafColor = "white";
    var minZoomDuration = 1500,
        zoomFactorCircle = 2.05,
        zoomFactorRole = 2.2;

    var hoverCircleColor =  "black",
        hoverCircleWidth = 1.5; // waring, can brake stroke with canvas drawing.

    var tooltipCss = `<style>
#nodeTooltip:after {
    content: "";
    position: absolute;
    top: 100%; /* This will position the arrow at the bottom of the tooltip */
    left: 50%;
    margin-left: -6px;
    border-width: 6px;
    border-style: solid;
    border-color: #555 transparent transparent transparent; /* This will make the top border black*/
    }
</style>`;

    //////////////////////////////////////////////////////////////
    ////////////////// Create Set-up variables  //////////////////
    //////////////////////////////////////////////////////////////

    // Get the chart div
    var $canvasParent = document.getElementById(canvasParentId);

    // Add the tooltip
    //var $tooltip = document.getElementById('nodeTooltip');
    var $tooltip = document.createElement('div');
    document.body.appendChild($tooltip);
	$tooltip.setAttribute('id', 'nodeTooltip');

	$tooltip.style.position = "absolute";
	$tooltip.style.textAlign = "center";
	$tooltip.style.background = "#555";
	$tooltip.style.color = "white";
	$tooltip.style.paddingLeft = "5px";
	$tooltip.style.paddingRight = "5px";
	$tooltip.style.paddingTop = "1px";
	$tooltip.style.paddingbottom = "1px";
	$tooltip.style.borderRadius = "4px";
	$tooltip.style.borderWidth = "1px";
	$tooltip.style.borderStyle = "solid";
	$tooltip.style.borderColor = "black";

    var minWidth = 400;
    var minHeight = 400;
    var computedWidth = $canvasParent.offsetWidth;
    //var computedWidth = parseInt(window.getComputedStyle($canvasParent).width, 10);
    var computedHeight = (window.innerHeight)/2;

    var width = Math.max(computedWidth, minWidth),
        height = Math.max(computedHeight, minHeight);
        //height = (computedHeight > computedWidth ?  computedWidth: computedHeight );

    var mobileSize = (window.innerWidth < 768 ? true : false);

    var centerX = width/2,
        centerY = height/2;
    //////////////////////////////////////////////////////////////
    /////////////////////// Create SVG  //////////////////////////
    //////////////////////////////////////////////////////////////

    //Create the visible canvas and context
    var canvas = d3.select("#"+canvasParentId).append("canvas")
        .attr("id", canvasId)
        .attr("width", width)
        .attr("height", height);
    var context = canvas.node().getContext("2d");
    setpixelated(context, true);
    context.clearRect(0, 0, width, height);

    // Set height of parent sibling
    var $nextToChart = document.getElementById('nextToChart')
    $nextToChart.style.minHeight = 2*height+"px";
    $nextToChart.style.display = "flex";
    $nextToChart.style.flexDirection = "column";
    //$nextToChart.style.overflowY = "auto";

    //Create a hidden canvas in which each circle will have a different color
    //We can use this to capture the clicked on circle
    var hiddenCanvas = d3.select("#"+canvasParentId).append("canvas")
        .attr("id", hiddenCanvasId)
        .attr("width", width)
        .attr("height", height)
        .style("display","none");
    var hiddenContext = hiddenCanvas.node().getContext("2d");
    hiddenContext.clearRect(0, 0, width, height);

    var $canvas = document.getElementById(canvasId);
    var $hidden_canvas = document.getElementById(hiddenCanvasId);

    //Create a custom element, that will not be attached to the DOM, to which we can bind the data
    var detachedContainer = document.createElement("custom");
    var dataContainer = d3.select(detachedContainer);

    //////////////////////////////////////////////////////////////
    /////////////////////// Create Scales  ///////////////////////
    //////////////////////////////////////////////////////////////

    var colorCircle = d3.scaleOrdinal()
        .domain(Array.from({length:colorCircleRange.length},(v,k)=>k))
        .range(colorCircleRange);

    var diameter = Math.min(width*0.97, height*0.97),
        radius = diameter / 2;

    var zoomInfo = {
        centerX: width / 2,
        centerY: height / 2,
        scale: 1
    };

    //Dataset to swich between color of a circle (in the hidden canvas) and the node data
    var colToCircle = {};

    //////////////////////////////////////////////////////////////
    ////////////////// Create Circle Packing /////////////////////
    //////////////////////////////////////////////////////////////

    // hack dataset (do that in the backend!?)
    var _d = hackDepth(dataset);
    var maxdepth = _d.maxdepth;

    var pack = d3.pack()
        .padding(1)
        .size([diameter, diameter])
    (d3.hierarchy(dataset)
        .sum(d => 10000/(maxdepth)**(Math.max(1.5,d.depth))) // d.neigbor // node size
        .sort((a, b) => 0)); //a.id < b.ID // node order

    var root = dataset;
    var nodes = pack.descendants(root);

    root = nodes[0];
    focus = root;
    hovered = null;

    //////////////////////////////////////////////////////////////
    ///////////////// Helpers function ///////////////////////
    //////////////////////////////////////////////////////////////

    function getNodeAttr(node) {
        var node_center_x,
            node_center_y,
            rayon;
        node_center_x = ((node.x - zoomInfo.centerX) * zoomInfo.scale) + centerX;
        node_center_y = ((node.y - zoomInfo.centerY) * zoomInfo.scale) + centerY;
        if (node.data.type == "role") {
            rayon = node.r * 0.95;
        } else {
            rayon = node.r;
        }

        return {node_center_x, node_center_y, rayon};
    }

    //////////////////////////////////////////////////////////////
    ///////////////// Canvas draw function ///////////////////////
    //////////////////////////////////////////////////////////////

    var cWidth = canvas.attr("width");
    var cHeight = canvas.attr("height");
    var nodeCount = nodes.length;

    var backgoundGrd = context.createLinearGradient(0, 0, cWidth, 0);
	backgoundGrd.addColorStop(0, colorDarker1);
	backgoundGrd.addColorStop(1, colorDarker2);

    //The draw function of the canvas that gets called on each frame
    function drawCanvas(ctx, hidden) {

        //Clear canvas
        ctx.fillStyle = backgoundGrd;
        ctx.rect(0,0,cWidth,cHeight);
        ctx.fill();

        //Select our dummy nodes and draw the data to canvas.
        var node = null;
        // It's slightly faster than nodes.forEach()
        for (var i = 0; i < nodeCount; i++) {
            node = nodes[i];

            if (node.data.type == undefined) node.data.type = "circle";

            var _name = node.data.name,
                _type = node.data.type,
                nattr = getNodeAttr(node);

            //If the hidden canvas was send into this function and it does not yet have a color, generate a unique one
            var circleColor,
                rayon;
            if(hidden) {
                if(node.color == null) {
                    // If we have never drawn the node to the hidden canvas get a new color for it and put it in the dictionary.
                    node.color = genColor();
                    colToCircle[node.color] = node;
                }
                // On the hidden canvas each rectangle gets a unique color.
                circleColor = node.color;
            } else {
                circleColor = node.children ? colorCircle(node.depth) : leafColor;
            }

            rayon = nattr.rayon * zoomInfo.scale;

            //Draw each circle
            ctx.beginPath();
            ctx.fillStyle = circleColor;
            ctx.arc(nattr.node_center_x, nattr.node_center_y,
                rayon, 0,  2 * Math.PI, true);
            ctx.fill();

            if (!hidden) {

                if (node.isHovered) {
                    ctx.lineWidth = hoverCircleWidth;
                    ctx.strokeStyle = hoverCircleColor;
                    ctx.stroke();
                }

                if (_type === "role") {
                    var text = _name.substring(0,2).replace(/./,x=>x.toUpperCase())
                    var font_size = 19;
                    var text_display = false;
                    //for (var ii=0; ii < 2; ii++) {
                    // Search font that fit
                    ctx.font = font_size +"px Arial";
                    if (ctx.measureText(text).width+1 < rayon*2) {
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
                        ctx.fillText(text, nattr.node_center_x, nattr.node_center_y+7);
                        //ctx.shadowColor = '#999';
                        //ctx.shadowBlur = 20;
                        //ctx.shadowOffsetX = 5;
                        //ctx.shadowOffsetY = 5;
                        ctx.fill();
                    }
                } else {
                    //if (focus.depth == node.depth || focus.depth == node.depth-1 ) {
                    //    ctx.beginPath();
                    //    ctx.fillStyle = "white";
                    //    ctx.fillCircleText(_name,
                    //        nattr.node_center_x, nattr.node_center_y,
                    //        rayon, -Math.PI*0.7);
                    //    ctx.fill()
                    //}
                }
            }
        }//for i
    }//function drawCanvas

    //////////////////////////////////////////////////////////////
    /////////////////// Click functionality //////////////////////
    //////////////////////////////////////////////////////////////

    // @DEBUG: d3.event.preventDefault(); ??

    function getNodeUnderPointer(e) {
        // We actually only need to draw the hidden canvas when there is an interaction.
        // This sketch can draw it on each loop, but that is only for demonstration.
        drawCanvas(hiddenContext, true);

        //Figure out where the mouse click occurred.
        var rect = $canvas.getBoundingClientRect();
        var mouseX = (e.layerX - rect.left);
        var mouseY = (e.layerY - rect.top);

        // Get the corresponding pixel color on the hidden canvas and look up the node in our map.
        // This will return that pixel's color
        var col = hiddenContext.getImageData(mouseX, mouseY, 1, 1).data;
        //Our map uses these rgb strings as keys to nodes.
        var colString = "rgb(" + col[0] + "," + col[1] + ","+ col[2] + ")";
        var node = colToCircle[colString];
        return node;
    }

    // Listen for clicks on the main canvas
    document.getElementById(canvasId).addEventListener("click", function(e){
        var node = getNodeUnderPointer(e);
        var zoomFactor = zoomFactorCircle;
        var isUpdated = false;
        if (node) {
            if (node.data.type === 'role') {
                var zoomFactor = zoomFactorRole;
            }

            if (focus === node) {
                // got to the parent node
                if (node !== root) {
                    node = node.parent;
                }
            }
            zoomToCanvas(node, zoomFactor);
            isUpdated = true;
        }

        if (isUpdated) {
            $tooltip.style.display = "none";
            var path = pack.path(node).map(n => {
                return { name: n.data.name,
                    nidjs: n.color };
            });
            app.ports.receiveData.send({
                nidjs:node.color,
                name:node.data.name,
                nodeType:node.data.type,
                path:path
            });
        }

        // doest work !?
        e.preventDefault();
        return false;
    });

    ///////////////////////////////////////////////////////////////
    /////////////////// Hoover functionality //////////////////////
    ///////////////////////////////////////////////////////////////

    // Listen for mouse moves on the main canvas
    document.getElementById(canvasId).addEventListener("mousemove", function(e){
        var node = getNodeUnderPointer(e);
        var ctx = context;
        //if (node && node !== root) {
        if (node) {
            if (node !== hovered) {
                if (hovered) {
                    // ==  clean hovered node + tooltip
                    var nattr = getNodeAttr(hovered);
                    ctx.beginPath();
                    ctx.arc(nattr.node_center_x, nattr.node_center_y,
                        nattr.rayon * zoomInfo.scale+1, 0, 2 * Math.PI, true);
                    ctx.lineWidth = 3;
                    ctx.strokeStyle = colorCircle(hovered.depth-1);
                    ctx.stroke();
                    hovered.isHovered = false;
                    $tooltip.style.display = "none";
                }

                // == add hovered circle
                var nattr = getNodeAttr(node);
                ctx.beginPath();
                ctx.arc(nattr.node_center_x, nattr.node_center_y,
                    nattr.rayon * zoomInfo.scale+1, 0,  2 * Math.PI, true);
                ctx.lineWidth = hoverCircleWidth;
                ctx.strokeStyle = hoverCircleColor;
                ctx.stroke();
                node.isHovered = true;

                // == add tooltip
                var rect = $canvas.getBoundingClientRect();
                $tooltip.style.display = "block";
                $tooltip.textContent = node.data.name;
                var tw = ($tooltip.clientWidth);
                var hw = (2*nattr.rayon * zoomInfo.scale + $tooltip.clientHeight);
                $tooltip.style.left = (nattr.node_center_x + rect.left - (tw/2 + 1)) + "px";
                $tooltip.style.top = (nattr.node_center_y + rect.top - (hw/2 + 21)) + "px";
                $tooltip.innerHTML += tooltipCss;

                hovered = node;
            }
        } else {
            if (hovered) {
                // == clean hovered node + tooltip
                var nattr = getNodeAttr(hovered);
                ctx.beginPath();
                ctx.arc(nattr.node_center_x, nattr.node_center_y,
                    nattr.rayon * zoomInfo.scale+1, 0, 2 * Math.PI, true);
                ctx.lineWidth = 3;
                ctx.strokeStyle = colorCircle(hovered.depth-1);
                ctx.stroke();
                hovered.isHovered = false;
                hovered = null;
                $tooltip.style.display = "none";
            }
        }
    });

    document.getElementById(canvasId).addEventListener("mouseleave", function(e){
        var ctx = context;
        if (hovered) {
            // == clean hovered node + tooltip
            var nattr = getNodeAttr(hovered);
            ctx.beginPath();
            ctx.arc(nattr.node_center_x, nattr.node_center_y,
                nattr.rayon * zoomInfo.scale+1, 0, 2 * Math.PI, true);
            ctx.lineWidth = 3;
            ctx.strokeStyle = colorCircle(hovered.depth-1);
            ctx.stroke();
            hovered.isHovered = false;
            hovered = null;
            $tooltip.style.display = "none";
        }
    });

    //////////////////////////////////////////////////////////////
    ///////////////////// Zoom Function //////////////////////////
    //////////////////////////////////////////////////////////////

    //Based on the generous help by Stephan Smola
    //http://bl.ocks.org/smoli/d7e4f9199c15d71258b5

    var ease = d3.easePolyInOut.exponent(3),
        timeElapsed = 0,
        interpolator = null,
        vOld = [focus.x, focus.y, focus.r * zoomFactorCircle];

    //Create the interpolation function between current view and the clicked on node
    function zoomToCanvas(focusNode, zoomFactor) {
        focus = focusNode;
        var v = [focus.x, focus.y, focus.r * zoomFactor]; //The center and width of the new "viewport"

        interpolator = d3.interpolateZoom(vOld, v); //Create interpolation between current and new "viewport"

        duration = Math.max(interpolator.duration, minZoomDuration); //Interpolation gives back a suggested duration
        timeElapsed = 0; //Set the time elapsed for the interpolateZoom function to 0
        vOld = v; //Save the "viewport" of the next state as the next "old" state

        var	dt = 0;
        var t = d3.timer(function(elapsed) {
            //stats.begin();
            var finished = interpolateZoom(elapsed - dt);
            dt = elapsed;
            drawCanvas(context);
            //stats.end();
            if (elapsed >= 1000 && finished) {
                t.stop();
            }
        });

    }//function zoomToCanvas

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
    //////////////////// Other Functions /////////////////////////
    //////////////////////////////////////////////////////////////

    //Generates the next color in the sequence, going from 0,0,0 to 255,255,255.
    //From: https://bocoup.com/weblog/2d-picking-in-canvas
    var nextCol = 1;
    function genColor(){
        var ret = [];
        // via http://stackoverflow.com/a/15804183
        if(nextCol < 16777215){
            ret.push(nextCol & 0xff); // R
            ret.push((nextCol & 0xff00) >> 8); // G
            ret.push((nextCol & 0xff0000) >> 16); // B

            nextCol += 100; // This is exagerated for this example and would ordinarily be 1.
        }
        var col = "rgb(" + ret.join(',') + ")";
        return col;
    }//function genColor

    //////////////////////////////////////////////////////////////
    /////////////////////// FPS Stats box ////////////////////////
    //////////////////////////////////////////////////////////////

    //var stats = new Stats();
    //stats.setMode(0); // 0: fps, 1: ms, 2: mb

    //// align top-left
    //stats.domElement.style.position = 'absolute';
    //stats.domElement.style.left = '0px';
    //stats.domElement.style.top = '0px';

    //document.body.appendChild( stats.domElement );

    //////////////////////////////////////////////////////////////
    /////////////////////// Initiate /////////////////////////////
    //////////////////////////////////////////////////////////////

    //First zoom to get the circles to the right location
	// then timer the interpolateZoom and rendering
    zoomToCanvas(root, zoomFactorCircle);
    //drawCanvas(context);


    // @DEBUG: Do not implemented
    // Implement redrawCanvas() !!!
    window.onresize = function () {
        //$canvas.style.width = '100%';
        //$canvas.style.height = canvas.style.height * .75;

        //redrawCanvas(focus);
        //drawAll(app, dataset);
        console.log("redrawCanvas not implemented yet !")
    }


    app.ports.sendNodeFocus.subscribe(function(nid) {
        var zoomFactor = zoomFactorCircle;
        zoomToCanvas(colToCircle[nid], zoomFactor);
    });

}//drawAll
