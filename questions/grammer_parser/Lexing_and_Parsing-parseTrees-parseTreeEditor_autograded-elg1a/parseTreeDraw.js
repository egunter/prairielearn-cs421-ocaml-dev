var jInstance = jsPlumb.getInstance();

var endpointOptions = {
  isSource: true,
  isTarget: true,
  endpoint: "Blank",
  connector: ["Straight"]
};

jInstance.ready(function() {
  // jInstance defaults
  jInstance.Defaults.Container = $("#viewport");
  jInstance.DefaultDragOptions = { cursor: "pointer", zIndex: 2000 };
  jInstance.endpointClass = "Blank";
  jInstance.connectorClass = "StateMachine";


  // On drag of a node, we want to check that we're not going to go above
  // parents, below children.
  $(document).on('drag', '.node', function(e) {
    // Source: Arrow pointing out
    // Target: Arrow pointing in
    var thisId = $(this).attr('id');
    serialize();
  });

  $(document).on('dragstart', '.node', function(e) {
    var that = this;
    $('#' + $(this).attr('id')).draggable(
        'option',
        'containment',
        'parent');
    console.log('fds');
    console.log(this);

    recalculateBoundaries();

    setTimeout(function() {
      var thatId = $(that).attr('id');
      //console.log(window.boundaries[thatId]);
      $(that).draggable(
        'option',
        'containment',
        [$(that).parent().offset().left + 1,
         window.boundaries[thatId]['top'],
         $(that).parent().outerWidth() +
           $(that).parent().offset().left -
           $(that).outerWidth() - 1,
         window.boundaries[thatId]['bottom']
        ]
      );
    }, 100);
  });


  $(document).on('DOMCharacterDataModified', '.node', function(e) {
    var thisId = $(this).attr('id');
    if ($('#' + thisId).text() != $(this).text()) {
      $('#' + thisId).text($(this).text());
    }
  });

  $(document).on('dragstop', '.node', function(e) {
    recalculateBoundaries();
  });

  $('#noProofTreeBox').on('change', function() {
    serialize();
  });

  jInstance.bind("click", function(conn) {
    recalculateBoundaries();
    var sourceId = conn.sourceId;
    var targetId = conn.targetId;
    var sourceIdId = '#' + conn.sourceId;
    var targetIdId = '#' + conn.targetId;
    $(sourceIdId).addClass('selectedDeleteEdge');
    $(targetIdId).addClass('selectedDeleteEdge');
    if (confirm("Do you wish to delete the edge between these two" + 
          " highlighted nodes?")) {
      var index = window.adjMatrix[sourceId].indexOf(targetId);
      if (index > -1 ) {
        window.adjMatrix[sourceId].splice(index, 1);
      }
      recalculateBoundaries();
      jInstance.detach(conn);
    }
    $(sourceIdId).removeClass('selectedDeleteEdge');
    $(targetIdId).removeClass('selectedDeleteEdge');
  });

  // On doubleclick of a connection, detach it.
  jInstance.bind("dblclick", function(conn) {
    var sourceId = conn.sourceId;
    var targetId = conn.targetId;
    for (var i = 0; i < adjMatrix.length; i++) {
      var index = window.adjMatrix[sourceId].indexOf(targetId);
      if (index > -1) {
        window.adjMatrix[sourceId].splice(index, 1);
      }
    }

    recalculateBoundaries();
    jInstance.detach(conn);
  });

  // On doubleclick of the viewport (and only the viewport), add a new node.
  $('#viewport').on('dblclick', function(e) {
    if (e.target == this && !e.shiftKey) {
      var timestamp = milliseconds();
      console.log(e);
      $(this).append('<div class="node" id="node' + timestamp + '" ' +
          'style="left:' + (e.pageX - $(this).offset().left) + 'px;top: ' +
          (e.pageY - $(this).offset().top) + 'px;" ' +
          'contentEditable=true>' +
          '</div>');
      /*
      $(this).append('<div class="node" id="node' + timestamp + '" ' +
          'style="left:' + e.offsetX + 'px;top: ' + e.offsetY + 'px;" ' +
          'contentEditable=true>' +
          '</div>');
      */
      nodeId = 'node' + timestamp;
      nodes.push(nodeId);
      window.adjMatrix[nodeId] = [];
      window.boundaries[nodeId] = {
        'top': $(this).offset().top + 1,
        'bottom': $(this).offset().top + $(this).height() - $('#' + nodeId).outerHeight()
      };

      jInstance.draggable($('#' + nodeId), {
        containment: "parent"
      });

      $('#' + nodeId).trigger('dragstart');
      $('#' + nodeId).trigger('drag');
      serialize();

      recalculateBoundaries();
    }
  });
});

  $(document).on('input', '.node', function(e) {
    serialize();
  });

  // Only when you select a node and click on the delete button does it
  // delete the node. Only works when there's one selected node.
  $(document).on('click', '#deleteNode', function(e) {
    if ($('.selected').length == 1 &&
        confirm("Are you sure you want to delete this node and its" +
          "associated edges?")) {
      var nodeId = $('.selected').attr('id');
      delete window.adjMatrix[nodeId];
      var adjMatrixKeys = Object.keys(window.adjMatrix);

      // For every node I have, make sure my adjacency list doesn't contain
      // the node I have just deleted.
      for(var i = 0; i < adjMatrixKeys.length; i++) {
        var index = window.adjMatrix[adjMatrixKeys[i]].indexOf(nodeId);
        if (index > -1) {
          window.adjMatrix[adjMatrixKeys[i]].splice(index, 1);
        }
      }

      // Clean up my list of nodes that I have.
      var indexOfNode = nodes.indexOf(nodeId);
      nodes.splice(indexOfNode, 1);
      jInstance.remove($('#' + nodeId));
      $('#' + nodeId).remove();
      serialize();
      recalculateBoundaries();
    }
  });

  // When we shift-click, we select a node. Selecting two nodes creates an
  // edge in our DAG.
$(document).on('click', '.node', function(e) {
  // This handles the appearance of our node.
  // This also takes care of jsplumb business of having tops & bottoms.
  recalculateBoundaries();
  if ($(this).hasClass('selected')) {
    $(this).removeClass('selected');
  } else {
    $(this).addClass('selected');

    var isSelected = $('.selected');

    if (e.shiftKey) {
      // If we have two nodes selected, make the top one the source and
      // the bottom the target. (The naming is complicated here.)
      if (isSelected.length == 2) {
        var selectedTop = isSelected[0];
        var selectedBottom = isSelected[1];
        if (isSelected[0].offsetTop < isSelected[1].offsetTop) {
          selectedTop = isSelected[1];
          selectedBottom = isSelected[0];
        }

        // Create endpoints for jInstance to attach a connector onto.
        var topEndpoint = jInstance.addEndpoint(
            selectedTop, { anchor:"TopCenter" }, endpointOptions );
        var bottomEndpoint = jInstance.addEndpoint(
            selectedBottom, { anchor:"BottomCenter" }, endpointOptions );

        var allConnections = jInstance.getAllConnections();

        var connectionExisted = false;

        // Don't create a connection if we already have one.
        if ('jsPlumb_DefaultScope' in allConnections) {
          allConnections = allConnections.jsPlumb_DefaultScope;
          for (var i = 0; i < allConnections.length; i++) {
            var conn = allConnections[i];
            if ((conn.target[0] == selectedTop && conn.source[0] == selectedBottom) ||
                (conn.target[0] == selectedBottom && conn.source[0] == selectedTop)) {
              connectionExisted = true;
            }
          }
        }

        // If there's no pre-existing connection, create it, then add it
        // to our adjacency list.
        if (!connectionExisted) {
          jInstance.connect({
            source: bottomEndpoint,
            target: topEndpoint,
            paintStyle:{ lineWidth:5, strokeStyle:'black' }
          });
          window.adjMatrix[$(selectedBottom).attr('id')].push($(selectedTop).attr('id'));
          recalculateBoundaries();
          $(selectedTop).trigger('dragstart');
          $(selectedTop).trigger('drag');
          $(selectedBottom).trigger('dragstart');
          $(selectedBottom).trigger('drag');
          serialize();
        }

        isSelected.removeClass('selected');
        jInstance.repaint()
      }
    } else {
      if (isSelected.length == 2) {
        isSelected.removeClass('selected');
        $(this).addClass('selected')
      }
    }
  }

  serialize();
  recalculateBoundaries();
  e.preventDefault();
  e.stopPropagation();
});

function serialize() {
  var resultsPackage = {};
  resultsPackage['noParseTree'] = $('#noProofTreeBox').prop('checked');
  resultsPackage['parseTree'] = {};
  for (var i = 0; i < window.nodes.length; i++) {
    var node = window.nodes[i];
    var list = window.adjMatrix[node];
    var nodeVal = $('#' + node).text();
    nodeVal = nodeVal.replace(/</g, '&lt;').replace(/>/g, '&gt;');

    resultsPackage['parseTree'][node] = {
      'to': list,
      'label': nodeVal,
      'meta': {
        'xPos': $('#' + node).css('left').split('px')[0],
        'yPos': $('#' + node).css('top').split('px')[0]
      }
    }
  }

  $('#serializedTree').val(JSON.stringify(resultsPackage));
  var keypress = jQuery.Event('input');
  $('#serializedTree').click().trigger(keypress).blur();

  return resultsPackage;
}

function deserialize() {
  $('#viewport').children('.node').remove();
  var studentSubmission = $('#serializedTree').val();

  if (studentSubmission.length > 0) {
    console.log(studentSubmission);
    studentSubmission = JSON.parse(studentSubmission);

    if ('parseTree' in studentSubmission) {
      var tree = studentSubmission['parseTree'];
      var nodes = Object.keys(tree);
      window.nodes = nodes;

      // Reappend each node
      for (var i = 0; i < nodes.length; i++) {
        var node = nodes[i];
        $('#viewport').append('<div class="node" id="' + node + '" ' +
            'style="left:' + tree[node]['meta']['xPos'] + 'px; ' +
            'top: ' + tree[node]['meta']['yPos'] + 'px;" ' +
            'contentEditable=true>' +
              tree[node]['label'] +
            '</div>');
        window.boundaries[node] = {};
        window.boundaries[node]['top'] = $('#viewport').offset().top;
        window.boundaries[node]['bottom'] =
          $('#viewport').offset().top + $('#viewport').innerHeight();

        jInstance.draggable($('#' + node), {
          containment: "parent"
        });
      }

      // Now that we have every node, go through the list again
      // and remake all the edges.
      for (var i = 0; i < nodes.length; i++) {
        var node = nodes[i];
        var selectedTop = $('#' + node);
        var targets = tree[node]['to'];
        window.adjMatrix[node] = targets;

        if (targets.length > 0) {
          for (var j = 0; j < targets.length; j++) {
            var selectedBottom = $('#' + targets[j]);
            var topEndpoint = jInstance.addEndpoint(
                selectedTop, { anchor:"BottomCenter" }, endpointOptions );
            var bottomEndpoint = jInstance.addEndpoint(
                selectedBottom, { anchor:"TopCenter" }, endpointOptions );

            jInstance.connect({
              source: topEndpoint,
              target: bottomEndpoint,
              paintStyle:{ lineWidth:5, strokeStyle:'black' }
            });
          }
        }

        // TODO fix this for the bottom node
        $('#' + node).trigger('dragstart');
        $('#' + node).trigger('drag');
      }

      jInstance.repaint()

      // Then remake the bounding boxes for the nodes.
      window.setTimeout(function() {
        recalculateBoundaries();
      }, 100);
    } else {
      // We don't have a parse tree, so bail.
    }
  }
}

function recalculateBoundaries() {
  var newBoundaries = {};
  var allConnections = jInstance.getAllConnections();
  if ('jInstance_DefaultScope' in allConnections) {
    allConnections = allConnections.jInstance_DefaultScope;

    for (var i = 0; i < allConnections.length; i++) {
      var conn = allConnections[i];
      var sourceId = conn.sourceId;
      var targetId = conn.targetId;
      var sourceIdId = '#' + conn.sourceId;
      var targetIdId = '#' + conn.targetId;

      if (!(sourceId in newBoundaries)) {
        newBoundaries[sourceId] = {};
        newBoundaries[sourceId]['top'] = $('#viewport').offset().top;
        newBoundaries[sourceId]['bottom'] =
          $(targetIdId).offset().top - (2 * $(sourceIdId).outerHeight());
      }

      if (!(targetId in newBoundaries)) {
        newBoundaries[targetId] = {};
        newBoundaries[targetId]['top'] = $(sourceIdId).offset().top + (2 * $(sourceIdId).outerHeight());
        newBoundaries[targetId]['bottom'] =
          $('#viewport').height() + $('#viewport').offset().top - $(targetIdId).outerHeight();
      }

      newBoundaries[sourceId]['bottom'] = Math.min(
        newBoundaries[sourceId]['bottom'],
        $(targetIdId).offset().top - (2 * $(sourceIdId).outerHeight())
      )

      newBoundaries[targetId]['top'] = Math.max(
        newBoundaries[targetId]['top'],
        $(sourceIdId).offset().top + (2 * $(sourceIdId).outerHeight())
      );

      var conn = allConnections[i];

      var sourceY = $('#' + sourceId).css('top').split('px')[0];
    }

    window.boundaries = newBoundaries;
  }
}

$(document).ready( function() {
  deserialize();
});
