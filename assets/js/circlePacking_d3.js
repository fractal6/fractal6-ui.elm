
function setpixelated(ctx, v){
    ctx['imageSmoothingEnabled'] = v;       /* standard */
    ctx['mozImageSmoothingEnabled'] = v;    /* Firefox */
    ctx['oImageSmoothingEnabled'] = v;      /* Opera */
    ctx['webkitImageSmoothingEnabled'] = v; /* Safari */
    ctx['msImageSmoothingEnabled'] = v;     /* IE */
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

    //var colorCircleRange = ['#bfbfbf','#838383','#4c4c4c','#1c1c1c', '#000000'];
    var colorCircleRange = ['#d9d9d9','#838383','#4c4c4c','#1c1c1c', '#000000'];

    var canvasParentId = "chart";
    var canvasId = "canvasOrga";
    var hiddenCanvasId = "hiddenCanvasOrga";
    var leafColor = "white",
        minZoomDuration = 1500,
        zoomFactorCircle = 2.05,
        zoomFactorRole = 2.2,
        hoverCircleColor =  "black",
        hoverCircleWidth = 1.5; // waring, can brake stroke with canvas drawing.

    //////////////////////////////////////////////////////////////
    ////////////////// Create Set-up variables  //////////////////
    //////////////////////////////////////////////////////////////

    // Get the chart div
    var $canvasParent = document.getElementById(canvasParentId);

    // Add the tooltip
    var $tooltip = document.getElementById('nodeTooltip');
	$tooltip.style.position = "absolute";
	$tooltip.style.textAlign = "center";
	$tooltip.style.background = "#555";
	$tooltip.style.color = "white";
	$tooltip.style.borderRadius = "3px";
	$tooltip.style.borderWidth = "3px";
	$tooltip.style.borderStyle = "solid";
	//$tooltip.style.borderColor = "transparent transparent transparent black";


    var minWidth = 400;
    var minHeight = 400;
    var computedWidth = $canvasParent.offsetWidth - 20;
    var computedHeight = (window.innerHeight)/2 - 20;

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
    var canvas  = d3.select("#"+canvasParentId).append("canvas")
        .attr("id", canvasId)
        .attr("width", width)
        .attr("height", height);
    var context = canvas.node().getContext("2d");
    setpixelated(context, true);
    context.clearRect(0, 0, width, height);

    //Create a hidden canvas in which each circle will have a different color
    //We can use this to capture the clicked on circle
    var hiddenCanvas  = d3.select("#"+canvasParentId).append("canvas")
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

    var diameter = Math.min(width*0.9, height*0.9),
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


    var pack = d3.pack()
        .padding(8)
        .size([diameter, diameter])
    (d3.hierarchy(dataset)
        .sum(d => d.size)
        .sort((a, b) => 0)); //a.id < b.id

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
            rayon = node.r * 0.8;
        } else {
            rayon = node.r;
        }

        return {node_center_x, node_center_y, rayon}
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

            var _name = node.data.name,
                _type = node.data.type;

            //If the hidden canvas was send into this function and it does not yet have a color, generate a unique one
            var circleColor;
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

            var nattr = getNodeAttr(node);

            //Draw each circle
            ctx.beginPath();
            ctx.fillStyle = circleColor;
            ctx.arc(nattr.node_center_x, nattr.node_center_y,
                nattr.rayon * zoomInfo.scale, 0,  2 * Math.PI, true);
            ctx.fill();
            if (node.isHovered) {
                ctx.lineWidth = hoverCircleWidth;
                ctx.strokeStyle = hoverCircleColor;
                ctx.stroke();
            }

            if (!hidden) {
                if (_type == "role") {
                    var displayName = _name.substring(0,2).replace(/./,x=>x.toUpperCase())
                    ctx.beginPath();
                    ctx.font = "22px Arial";
                    ctx.fillStyle = "black";
                    ctx.textAlign = "center";
                    ctx.fillText(_name.substring(0,3), nattr.node_center_x, nattr.node_center_y+7);
                    //ctx.shadowColor = '#999';
                    //ctx.shadowBlur = 20;
                    //ctx.shadowOffsetX = 5;
                    //ctx.shadowOffsetY = 5;
                    ctx.fill();
                }
            }
        }//for i
    }//function drawCanvas

    //////////////////////////////////////////////////////////////
    /////////////////// Click functionality //////////////////////
    //////////////////////////////////////////////////////////////

    // @DEBUG: d3.event.preventDefault(); ??

    // Listen for clicks on the main canvas
    document.getElementById(canvasId).addEventListener("click", function(e){
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


        var zoomFactor = zoomFactorCircle;
        var isUpdated = false;
        if (node) {
            if (node.data.type == 'role') {
                var zoomFactor = zoomFactorRole;
            }

            if (focus !== node) {
                zoomToCanvas(node, zoomFactor);
                isUpdated = true;
            } else {
                zoomToCanvas(root, zoomFactor);
                isUpdated = true;
                node = root;
            }
        }

        if (isUpdated) {
			$tooltip.style.display = "none";
            app.ports.receiveData.send({
                name:node.data.name,
                nodeType:node.data.type,
                nid:"0"});
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

    var ctx = context;
    if (node && node !== root) {
        if (node !== hovered) {
            if (hovered) {
                // ==  clean hovered node
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

            var rect = $canvas.getBoundingClientRect();
            $tooltip.style.display = "block";
            $tooltip.textContent = node.data.name;
            var tw = ($tooltip.clientWidth);
            var hw = (2*nattr.rayon * zoomInfo.scale + $tooltip.clientHeight);
            $tooltip.style.left = (nattr.node_center_x + rect.left - ( tw/2 + 1)) + "px";
            $tooltip.style.top = (nattr.node_center_y + rect.top - (hw/2 + 12)) + "px";

            hovered = node;
        }
    } else {
        if (hovered) {
            // == clean hovered node
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
                return false
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
        console.log("redrawCanvas not implemented yet !")
    }

}//drawAll
