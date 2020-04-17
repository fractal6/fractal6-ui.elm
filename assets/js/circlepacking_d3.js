// @Debug use import with a bundler/webpack !
//
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
// * cumchild: total number of child
// * neigbor: number of neogbor
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
    obj.children.forEach(function (d) {
      var d = computeDepth(d, currentdepth+1, obj.children.length);
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
  //////////////////////////////////////////////////////////////
  //////////////// Style Constants  ////////////////////////////
  //////////////////////////////////////////////////////////////

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

  // http://www.cssarrowplease.com/
  var tooltipCss = `<style>
  #nodeTooltip:after, #nodeTooltip:before{
      content: "";
      position: absolute;
      top: 100%; /* This will position the arrow at the bottom of the tooltip */
      left: 50%;
      pointer-events: none;
      border: solid transparent;
      }
  #nodeTooltip:after {
      margin-left: -6px;
      border-width: 6px;
      border-top-color: #eee;
  }
  #nodeTooltip:before {
      margin-left: -7px;
      border-width: 7px;
      border-top-color: #333;
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

	$tooltip.style.textAlign = "center";
	$tooltip.style.color = "black";
	$tooltip.style.background = "#eee";
	$tooltip.style.borderRadius = "4px";
	$tooltip.style.border = "1px solid #333";
	$tooltip.style.paddingLeft = "6px";
	$tooltip.style.paddingRight = "6px";
	$tooltip.style.paddingTop = "2px";
	$tooltip.style.paddingBottom = "2px";

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
  //////////// Create Circle Scales and Propertie /////////////
  //////////////////////////////////////////////////////////////
  var diameter = Math.min(width*0.97, height*0.97),
    radius = diameter / 2;

  var zoomInfo = {
    centerX: width / 2,
    centerY: height / 2,
    scale: 1
  };

  // Mapping function from a node depth to color.
  const colorCircle = d3.scaleOrdinal()
    .domain(Array.from({length:colorCircleRange.length},(v,k)=>k))
    .range(colorCircleRange);

  //////////////////////////////////////////////////////////////
  ////////////////// Create Circle Packing /////////////////////
  //////////////////////////////////////////////////////////////

  var gStats; // Receive graph global statistics
  var pack; // Receive D3 data structure
  var nodes; // List of d3 nodes
  var root; // The root node of the graph
  var focus; // The node that has the active focus
  var hovered; // The node that is curently hoovered
  // Dataset to swich between color of a circle (in the hidden canvas) and the node data
  var colToCircle = {};

  graph = formatGraph(graph);
  if (graph.length > 1) console.warn("More than 1 graph given -> Some nodes are not connected.")
  graph = graph[0]
  gStats = computeDepth(graph);
  //console.log(graph);

  pack = d3.pack()
    .padding(1)
    .size([diameter, diameter])
  (d3.hierarchy(graph)
    .sum(d => 10000/(gStats.maxdepth)**(Math.max(1.5, d.depth))) // d.neigbor // node size
    .sort((a, b) => 0)); //a.id < b.ID // node order

  nodes = pack.descendants(graph);
  root = nodes[0];
  focus = root;
  hovered = null;


  //////////////////////////////////////////////////////////
  ///////////////// Helpers function ///////////////////////
  //////////////////////////////////////////////////////////

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
  }

  // Get node position and properties
  function getNodeAttr(node) {
      var node_center_x,
          node_center_y,
          rayon;
      node_center_x = ((node.x - zoomInfo.centerX) * zoomInfo.scale) + centerX;
      node_center_y = ((node.y - zoomInfo.centerY) * zoomInfo.scale) + centerY;
      if (node.data.type_ === "Role") {
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

          var _name = node.data.name,
              type_ = node.data.type_,
              nattr = getNodeAttr(node);

          //If the hidden canvas was send into this function and it does not yet have a color, generate a unique one
          var circleColor,
              rayon;
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

          rayon = nattr.rayon * zoomInfo.scale;

          //Draw each circle
          ctx.beginPath();
          ctx.fillStyle = circleColor;
          ctx.arc(nattr.node_center_x, nattr.node_center_y,
              rayon, 0, 2 * Math.PI, true);
          ctx.fill();

          if (!hidden) {

              if (node.isHovered) {
                  ctx.lineWidth = hoverCircleWidth;
                  ctx.strokeStyle = hoverCircleColor;
                  ctx.stroke();
              }

              if (type_ === "Role") {
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
  /////////////////// Event Handler ////////////////////////////
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

  // Click event
  // Listen for clicks on the main canvas
  document.getElementById(canvasId).addEventListener("click", function(e){
      var node = getNodeUnderPointer(e);
      var zoomFactor = zoomFactorCircle;
      var isUpdated = false;
      if (node) {
          if (node.data.type_ === 'Role') {
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
              nodeType:node.data.type_,
              path:path
          });
      }

      // doest work !?
      e.preventDefault();
      return false;
  });

  // Hoover event
  // Listen for mouse moves/hooverin on the main canvas
  document.getElementById(canvasId).addEventListener("mousemove", function(e){
      var node = getNodeUnderPointer(e);
      var ctx = context;
      var clean_hover = true;
      //if (node && node !== root) {
      if (node) {
          if (node !== hovered) {
              if (hovered) {
                  // ==  clean hovered node + tooltip
                  var nattr = getNodeAttr(hovered);

                  var rect = $canvas.getBoundingClientRect();
                  var mouseX = (e.layerX - rect.left);
                  var mouseY = (e.layerY - rect.top);
                  var x1 = nattr.node_center_x - nattr.rayon;
                  var x2 = nattr.node_center_x + nattr.rayon;
                  var y1 = nattr.node_center_y + nattr.rayon - 10;
                  var y2 = nattr.node_center_y + nattr.rayon + 10;
                  clean_hover = !( (mouseY > y1) && (mouseY < y2) && (x1 < mouseX) && (mouseX < x2))
                  if (clean_hover) {
                          ctx.beginPath();
                          ctx.arc(nattr.node_center_x, nattr.node_center_y,
                              nattr.rayon * zoomInfo.scale+1, 0, 2 * Math.PI, true);
                          ctx.lineWidth = 3;
                          ctx.strokeStyle = colorCircle(hovered.depth-1);
                          ctx.stroke();
                          hovered.isHovered = false;
                          $tooltip.style.display = "none";
                      }
              }

              if (clean_hover) {
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
                  $tooltip.style.top = (nattr.node_center_y + rect.top - (hw/2 + 23)) + "px";
                  $tooltip.innerHTML += tooltipCss;

                  hovered = node;
              }
          }
      } else {
          if (hovered) {
              // == clean hovered node + tooltip
              var nattr = getNodeAttr(hovered);

              var rect = $canvas.getBoundingClientRect();
              var mouseX = (e.layerX - rect.left);
              var mouseY = (e.layerY - rect.top);
              var y1 = nattr.node_center_x - nattr.rayon;
              var y2 = nattr.node_center_x + nattr.rayon;
              var x1 = nattr.node_center_y + nattr.rayon -10;
              var x2 = nattr.node_center_y + nattr.rayon + 10;
              clean_hover = !( (mouseY > y1) && (mouseY < y2) && (x1 < mouseX) && (mouseX < x2))
              if (clean_hover) {
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
      }
  });

  // Hoover event
  // Listen for mouse moves/hooverout on the main canvas
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
  console.log("Canvas initalization");
  zoomToCanvas(root, zoomFactorCircle);
  //drawCanvas(context);


  // @DEBUG: Do not implemented
  // Implement redrawCanvas() !!!
  window.onresize = function () {
    //$canvas.style.width = '100%';
    //$canvas.style.height = canvas.style.height * .75;

    //redrawCanvas(focus);
    //drawAll(app, graph);
    console.log("redrawCanvas not implemented yet !")
  }


  app.ports.sendNodeFocus.subscribe(function(nid) {
    var zoomFactor = zoomFactorCircle;
    zoomToCanvas(colToCircle[nid], zoomFactor);
  });

}//drawAll
