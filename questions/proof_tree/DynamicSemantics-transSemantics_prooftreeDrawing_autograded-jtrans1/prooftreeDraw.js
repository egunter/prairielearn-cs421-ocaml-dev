/*****************************
 * prooftreeDraw.js
 * v1.1: Added support for serializing/deserializing
 * v1.0: Initial
 *
 * Written by Terence Nip
 ****************************/

/***************************
 * Event Listener: proofTreeButtonSave
 * Creates a new node in our proof tree and resizes.
 ***************************/
  $('#proofTreeButtonSave').on('click', function(e) {
    $('#proofTreeModalContainer').css('display', 'none');
    var parentId = $(this).attr('parentId');
    var nextLevel = $(this).attr('nextLevel');
    var currLevel = nextLevel - 1;
    var nextLevelText = "l" + nextLevel;

    //var rightSeparator = $('#proofTreeModalAnsCondition').val().length > 0 ? "where " : "";
    var style = '';
    var leftSeparator = '<span class="leftSeparator"></span>';

    var nestHtml = '<div class="proofTreeNest ' + nextLevelText + '">' +
      '<div class="proofTreeAddSubproof">' +
        '<a href="#" currLevel="' + nextLevel + '" class="addSubproof">[+]</a>' +
        '<span class="proofTreeSideConditionLink">' +
          ' | <a href="#" class="addSideCondition">[sc]</a>' +
        '</span>' +
      '</div>' +
      '<div class="proofTreeActions">' +
        '<a href="#" currLevel="' + currLevel + '" class="edit">[e]</a> | ' +
        '<a href="#" class="delete">[x]</a>' +
      '</div>' +
      '<input type="text" class="proofTreeSideCondition" placeholder="Side Condition" />' +
      '<div class="proofTreeLabel">' +
        '<select class="proofTreeDropdownLabel">' +
          '<option selected></option>' +
          '<option selected></option>' +
          '<option name="ident">Ident</option>' +
          '<option name="num">Num</option>' +
          '<option name="bool">BoolConst</option>' +
          '<option name="arith">BinOp</option>' +
          '<option name="and">And</option>' +
          '<option name="or">Or</option>' +
          '<option name="not">Not</option>' +
          '<option name="skip">Skip</option>' +
          '<option name="assign">Assign</option>' +
          '<option name="seq">Seq</option>' +
          '<option name="if">If</option>' +
          '<option name="while">While</option>' +
          '<option name="rel">Rel</option>' +
        '</select>' +
      '</div>' +
      '<div class="proofTreeLine" ' + style + '>' +
        '<span class="proofTreeLineLeft tt">' +
          $('#proofTreeModalAnsLeft').val() +
        '</span><br />' +
        leftSeparator +
        '<br /><span class="proofTreeLineRight tt">' +
          $('#proofTreeModalAnsRight').val() +
        '</span>' /*+
        '<span class="proofTreeLineConditionPhrase">' +
        rightSeparator +
        '</span><br />' +
         '<span class="proofTreeLineCondition tt">' +
           $('#proofTreeModalAnsCondition').val()  +
//           (($('#proofTreeButtonAddSideCond').css('display') == 'none') ? $('#proofTreeModalAnsCondition').val() : '')  +
        '</span>' */+
      '</div>' +
   '</div>';

    // Add the new subtree into the proof tree
    $('#' + parentId).children('.proofTreeAddSubproof').before(nestHtml);

    // Do the resizing and add the corresponding leftSeparator, rerun MathJax
    giveWidths($('#proofTreeContainer').children(), 'root', 0);
    loadLeftSeparator();
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);

    flipSideConditionLinkVisibility(e);

    // Kill normal event-age
    e.stopImmediatePropagation()
    e.preventDefault()
  });

/***************************
 * Event Listener: proofTreeButtonCancel
 * Closes the add proof tree modal.
 ***************************/
  $('#proofTreeButtonCancel').on('click', function(e) {
    $('#proofTreeModalContainer').css('display', 'none');
    e.stopImmediatePropagation()
    e.preventDefault()
  });

/***************************
 * Event Listener: a.edit
 * Opens the add proof tree modal with pre-filled values.
 ***************************/
  $(document).on('click', 'a.edit', function(e) {
    $('#proofTreeModalContainer').css('display', '');
    $('#proofTreeButtonSave').css('display', 'none');
    $('#proofTreeButtonEdit').css('display', 'inline');
    $('#proofTreeModal').css('display', '');
    $('.proofTreeForm').css('display', 'block');
    var currLevel = parseInt($(this).attr('currLevel'));
    var nextLevel = currLevel + 1;
    var nextLevelText = "l" + nextLevel;
    var currentId = $(this).parent().parent().attr('id');
    var parentId = $(this).parent().parent().parent().attr('id');

    var proofTreeLineLeft = $(this)
      .parent().parent()
      .children('.proofTreeLine').children('.proofTreeLineLeft').text();
    var proofTreeLineRight = $(this)
      .parent().parent()
      .children('.proofTreeLine').children('.proofTreeLineRight').text();
    var proofTreeLineCondition = $(this)
      .parent().parent()
      .children('.proofTreeLine').children('.proofTreeLineCondition').text();

    var previousProofTreeLineLeft = $('#' + parentId)
      .children('.proofTreeLine')
      .children('.proofTreeLineLeft').text();
    var previousProofTreeLineRight = $('#' + parentId)
      .children('.proofTreeLine')
      .children('.proofTreeLineRight').text();
    var previousProofTreeLineCondition = $('#' + parentId)
      .children('.proofTreeLine')
      .children('.proofTreeLineCondition').text();

    // Set values in the modal for usage
    $('#proofTreeModalAnsLeft').val(proofTreeLineLeft)
    $('#proofTreeModalAnsRight').val(proofTreeLineRight);
    $('#proofTreeModalAnsCondition').val(proofTreeLineCondition);

    $('#proofTreeModalLeft').text(previousProofTreeLineLeft);
    $('#proofTreeModalRight').text(previousProofTreeLineRight);
    $('#proofTreeModalCondition').text(previousProofTreeLineCondition);

    $('#proofTreeButtonEdit').attr('nextLevel', nextLevel);
    $('#proofTreeButtonEdit').attr('parentId', parentId);
    $('#proofTreeButtonEdit').attr('currentId', currentId);
    $('#proofTreeModalContainer').css('display', 'block');

    e.stopImmediatePropagation()
    e.preventDefault()
  });

/***************************
 * Event Listener: proofTreeButtonEdit
 * Updates the proof tree with the values in the modal.
 ***************************/
  $('#proofTreeButtonEdit').on('click', function(e) {
    $('#proofTreeModalContainer').css('display', 'none');
    var parentId = $(this).attr('parentId');
    var currentId = $(this).attr('currentId');
    var nextLevel = $(this).attr('nextLevel');
    var currLevel = nextLevel - 1;
    var nextLevelText = "l" + nextLevel;

    // By default, always change both L and R
    $('#' + currentId).children('.proofTreeLine').children('.proofTreeLineLeft').text($('#proofTreeModalAnsLeft').val())
    $('#' + currentId).children('.proofTreeLine').children('.proofTreeLineRight').text($('#proofTreeModalAnsRight').val())

    // Change the condition clause to blank by default
    var rightSeparator = '';

    // Only change the condition & its clause if the button is invisible.
    //if ($('#proofTreeButtonAddSideCond').css('display') == 'none') {
    $('#' + currentId).children('.proofTreeLine').children('.proofTreeLineCondition').text($('#proofTreeModalAnsCondition').val())
    rightSeparator = ($('#proofTreeModalAnsCondition').val().length > 0) ? ":" : "";
    console.log($('#proofTreeModalAnsCondition').val()) // This is just logging, but OK. ELG
    //} else {
   //?   $('#' + currentId).children('.proofTreeLine').children('.proofTreeLineCondition').text('')
    //}

    // Assign the condition clause
    $('#' + currentId + ' .proofTreeLineConditionPhrase').text(rightSeparator);

    // Do the resizing and add the corresponding leftSeparator, rerun MathJax
    giveWidths($('#proofTreeContainer').children(), 'root', 0);
    loadLeftSeparator();
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);

    // Kill normal event-age
    e.stopImmediatePropagation()
    e.preventDefault()
  });

/***************************
 * Event Listener: a.addSubproof
 * Fires up the add subproof modal with prefilled values as the bottom of
 * the rule.
 ***************************/
  $(document).on('click', 'a.addSubproof', function(e) {
    $('#proofTreeModalContainer').css('display', '');
    $('#proofTreeButtonSave').css('display', '');
    $('#proofTreeButtonEdit').css('display', 'none');
    $('#proofTreeModal').css('display', '');
    $('.proofTreeForm').css('display', 'block');
    var currLevel = parseInt($(this).attr('currLevel'));
    var nextLevel = currLevel + 1;
    var nextLevelText = "l" + nextLevel;
    var currentId = $(this).parent().parent().attr('id');

    var proofTreeLabelValue = $(this)
      .parent().parent()
      .children('.proofTreeLabel').children('select').val();

    var proofTreeLineLeft = $(this)
      .parent().parent()
      .children('.proofTreeLine').children('.proofTreeLineLeft').text();
    var proofTreeLineRight = $(this)
      .parent().parent()
      .children('.proofTreeLine').children('.proofTreeLineRight').text();
    var proofTreeLineCondition = $(this)
      .parent().parent()
      .children('.proofTreeLine').children('.proofTreeLineCondition').text();

    // Set values in the modal for usage
    $('#proofTreeModalAnsLeft').val('');
    $('#proofTreeModalAnsRight').val('');
    $('#proofTreeModalAnsCondition').val('');
    $('#proofTreeModalLeft').text(proofTreeLineLeft);
    $('#proofTreeModalRight').text(proofTreeLineRight);

    $('#proofTreeModalCondition').text(proofTreeLineCondition);
    $('#proofTreeButtonSave').attr('nextLevel', nextLevel);
    $('#proofTreeButtonSave').attr('parentId', currentId);
    $('#proofTreeModalContainer').css('display', 'block');

    $('#proofTreeModalLabel').text(proofTreeLabelValue);

    e.stopImmediatePropagation()
    e.preventDefault()
  });

/***************************
 * Event Listener: a.delete
 * Deletes the inference and ALL subproofs after confirming.
 ***************************/
  $(document).on('click', 'a.delete', function(e) {
    if (confirm("Please confirm that you wish to delete this inference AND its associated subtrees.")) {
      $(this).parent().parent().remove()
      giveWidths($('#proofTreeContainer').children(), 'root', 0);
      flipSideConditionLinkVisibility(e);
      var serializedTree = serializeTree();
    }
    e.stopImmediatePropagation()
    e.preventDefault()
  });

/***************************
 * Event Listener: proofTreeViewModalOpen
 * Opens a modal for users to view the tree in a larger form.
 ***************************/
  $('#proofTreeViewModalOpen').on('click', function(e) {
    $('#proofTreeModalContainer').css('display', 'block');

    var proofTreeView = $('.proofTreeView').css('display', 'block');
    $('#proofTreeViewModal').css('display', 'block').append(proofTreeView);
    $('#proofTreeViewModal').css('display', 'block');

    var proofTree = $('#proofTreeContainer').detach();
    $('#proofTreeViewModalClose').before(proofTree);
    giveWidths($('#proofTreeContainer').children(), 'root', 0);

    $('.proofTreeAddSubproof').css('display', 'none');
    $('.proofTreeActions').css('display', 'none');
    $('.proofTreeDropdownLabel').attr('disabled', 'disabled');
    $('.proofTreeDropdownLabel').css('background-color', '#AAA');
    $('.proofTreeDropdownLabel').css('color', '#000');

    $('.proofTreeSideCondition').attr('disabled', 'disabled');
    $('.proofTreeSideCondition').css('background-color', '#AAA');
    $('.proofTreeSideCondition').css('color', '#000');

    $('#proofTreeModal').css('display', 'none');
  });

/***************************
 * Event Listener: proofTreeViewModalOpen
 * Closes the modal allowing users to view the tree in a larger form.
 ***************************/
  $('#proofTreeViewModalClose').on('click', function(e) {
    $('#proofTreeModalContainer').css('display', 'none');
    $('#proofTreeViewModal').css('display', 'none')

    var proofTree = $('#proofTreeContainer').detach();
    $(proofTree).insertBefore('#proofTreeViewModalOpen');
    giveWidths($('#proofTreeContainer').children(), 'root', 0);

    $('.proofTreeAddSubproof').css('display', 'inline');
    $('.proofTreeActions').css('display', 'inline');

    $('.proofTreeDropdownLabel').removeAttr('disabled');
    $('.proofTreeDropdownLabel').css('background-color', '');
    $('.proofTreeDropdownLabel').css('color', '');

    $('.proofTreeSideCondition').removeAttr('disabled');
    $('.proofTreeSideCondition').css('background-color', '');
    $('.proofTreeSideCondition').css('color', '');

    $('#proofTreeModal').css('display', 'enabled');
  });

  $(document).on('change', '.proofTreeDropdownLabel', function(e) {
    var serializedTree = serializeTree();
  });

  $(document).on('change', '.proofTreeSideCondition', function(e) {
    var serializedTree = serializeTree();
  });

  // Primarily handles the modal when we resize the window.
  window.onresize = function() {
    $('#proofTreeModalContainer').height($(window).height() - $('.nav').height())
    $('#proofTreeModalContainer').css('margin-top', '-' + $('.navbar').css('margin-bottom'))
    giveWidths($('#proofTreeContainer').children(), 'root', 0);
  };

  $(document).on('keyup', 'input.proofTreeSideCondition', function(e) {
    serializeTree();
    e.stopImmediatePropagation()
    e.preventDefault()
  });

  $(document).on('click', 'a.addSideCondition', function(e) {
    var currentPosition = $(this).parent().parent().parent().attr('id');
    if ($('#' + currentPosition + ' > .proofTreeSideCondition').css('display') == 'none') {
      $('#' + currentPosition + ' > .proofTreeSideCondition').css('display', 'inline');
    } else {
      $('#' + currentPosition + ' > .proofTreeSideCondition').css('display', 'none');
    }
    e.stopImmediatePropagation()
    e.preventDefault()
  });

  $(document).ready(function() {
    // What we want to do is be able to define (per instance) what the leftSeparator
    // should be.
    //
    // TODO make this be dependent upon the question type.
    if (window.leftSeparator == undefined) {
      window.leftSeparator = "vdash";
    }

    // Re-runs the MathJax stuff after we dynamically insert the leftSeparator.
    loadLeftSeparator();
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);

    // This allows us to inject a modal on top of all the things.
    if($('#proofTreeModalContainer').length == 0) {
      $('nav')
        .after('<div id="proofTreeModalContainer"><div id="proofTreeViewModal" style="display:none;"></div><div id="proofTreeModal" style="display:none;"></div></div>');

      var proofTreeForm = $('.proofTreeForm').detach();
      $('#proofTreeModal').append(proofTreeForm);
    }

    // Handles the initial sizing of the modal.
    $('#proofTreeModalContainer').height($(window).height() - $('.nav').height())
    $('#proofTreeModalContainer').css('margin-top', '-' + $('.navbar').css('margin-bottom'))

    $('#content .proofTreeForm').remove();

    // Handles side condition stuff.
    if (window.fullQuestionName !== undefined &&
        window.fullQuestionName.indexOf('polyTy') >= 0) {
      $('.conditionText').css('display', 'none');
      $('#proofTreeModalAnsCondition').css('display', 'none');
    }

    $('#proofTreeModalContainer').css('display','none');
  });

/***************************
 * loadLeftSeparator
 * Inserts the appropriate leftSeparator into leftSeparator spans.
 ***************************/
  function loadLeftSeparator() {
    $('.leftSeparator').each(function() {
      $(this).html('$\\' + window.leftSeparator + '$');
    });
  }

/***************************
 * flipSideConditionLinkVisibility
 * Given an event, updates the visibility of side condition text boxes and SC
 * links.
 ***************************/
  function flipSideConditionLinkVisibility(evt) {
    /*
    // For all side condition things, hide the link.
    $('.proofTreeSideConditionLink').css('display', 'none');

    // Now, only show the link as appropriate.
    //var results = $('.proofTreeNest:not(:has(".proofTreeNest"))').each(function() {
    var results = $('#proofTreeContainer .proofTreeNest:not(:has(".proofTreeNest"))').each(function() {
      var noSubtreeId = $(this).attr('id');
      $('#' + noSubtreeId + ' .proofTreeSideConditionLink').css('display', 'inline');
    });

    var results = $('#proofTreeContainer .proofTreeNest:has(".proofTreeNest")').each(function() {
      var noSubtreeId = $(this).attr('id');
      console.log('haschild', noSubtreeId);
      $('#' + noSubtreeId + '> .proofTreeSideCondition').css('display', 'none');
      $('#' + noSubtreeId + '> .proofTreeSideCondition').val('');
    });
    console.log(results);
    console.log(evt);
    */
  }

/***************************
 * giveWidths
 * Assigns widths to each node in our proof tree.
 ***************************/
  function giveWidths(elems, hierarchy, currLevel) {
    var relevantElems = [];

    // An element is relevant if they have the same level ID.
    for (var i = 0; i < elems.length; i++) {
      if (elems[i].classList.contains('proofTreeNest')) {
        relevantElems.push(elems[i]);
      }
    }

    // elemCount is used for giving each "node" in our tree a unique ID.
    var elemCount = 0;
    var width = $('#proofTreeContainer').width();

    // For every level, style the elements accordingly.
    for (var i = 0; i < relevantElems.length; i++) {
      var currChar = String.fromCharCode(97 + elemCount);
      elems[i].id = hierarchy + '-l' + currLevel + currChar;

      if (elems[i].id == 'root-l0a') {
        width = $('#proofTreeContainer').width();
      } else {
        width = (Math.floor($(elems[0]).parent('.proofTreeNest').width() - 25) /
          relevantElems.length);
      }

      // Give the checkbox this ID!
      $(elems[i]).children('.proofTreeLabel')
        .children('input[type=checkbox]')
        .attr('data-checkedoptional', 'submittedAnswer.' + elems[i].id);

      var elem = $('#' + elems[i].id);
      elem.outerWidth(width + 'px');
      elem.css('display', 'table-cell');
      elem.css('vertical-align', 'bottom');
      elem.css('padding-left', '3px');
      elem.css('padding-right', '3px');
      if (elems[i].classList.contains('proofTreeNest')) {
        elem = elem.children('.proofTreeNest:last-child');
        elem.css('margin', '0');
        elem.css('text-align', 'center');
        elem.outerWidth(width + 'px');
        elem.css('display', 'table-cell');
        elem.css('vertical-align', 'bottom');
      }

      elemCount++;

      if ($(elems[i]).children('.proofTreeNest').length > 0) {
        giveWidths($(elems[i]).children('.proofTreeNest'), elems[i].id, currLevel + 1);
      }
    }

    // Every time we resize, we need to reserialize.
    serializeTree();
  }

/***************************
 * serializeTree
 * Serializes the tree for submission.
 ***************************/
  function serializeTree() {
    var result = {};

    $('#proofTreeContainer .proofTreeNest').each(function() {
      var treeLocation = $(this).attr('id');
      var lineLeft = $('#' + treeLocation +  ' > .proofTreeLine > .proofTreeLineLeft').text();
      var lineRight = $('#' + treeLocation + ' > .proofTreeLine >   .proofTreeLineRight').text();
      var lineCondition = $('#' + treeLocation + ' > .proofTreeLine  > .proofTreeLineCondition').text();
      var lineLabel = $('#' + treeLocation + ' > .proofTreeLabel >  .proofTreeDropdownLabel').val();
      var lineSideCondition = $('#' + treeLocation + ' > .proofTreeSideCondition').val();

      result[treeLocation] = {};
      result[treeLocation]['left'] = $.trim(lineLeft);
      result[treeLocation]['middle'] = $.trim(lineRight);
      result[treeLocation]['right'] = $.trim(lineCondition);
      result[treeLocation]['label'] = $.trim(lineLabel);
      result[treeLocation]['sideCondition'] = $.trim(lineSideCondition);
    });

    var keypress = jQuery.Event('input');
    $('#serializedTree').val(JSON.stringify(result));
    $('#serializedTree').click().trigger(keypress).blur();

    console.log(result);

    return result;
  }

  function unmarshal() {
    if (window.unmarshalled != undefined && !window.unmarshalled) {
      window.unmarshalled = true;
      var rawString = $('#serializedTree').val();
      console.log(rawString);
      var jsonTree = JSON.parse(rawString);
      var treePositions = Object.keys(jsonTree).sort();

      for (var i = 0; i < treePositions.length; i++) {
        var currentPosition = treePositions[i];
        var parentPosition = treePositions[i].split('-');
        parentPosition.pop();
        parentPosition = parentPosition.join('-');

        var currentPositionId = '#' + currentPosition;
        var parentPositionId = '#' + parentPosition;

        var labelVal = jsonTree[currentPosition]['label'];
        var leftVal = jsonTree[currentPosition]['left'];
        var middleVal = jsonTree[currentPosition]['middle'];
        var rightVal = jsonTree[currentPosition]['right'];
        var sideConditionVal = jsonTree[currentPosition]['sideCondition'];

        // We handle root-l0a separately from everything else.
        // More specifically, we only update the dropdown.
        if (parentPosition != "root" && $(currentPosition).length == 0) {
          // We need to get the current level in the tree - and as such, we need that
          // of the parent.
          console.log(parentPositionId, $(parentPositionId).attr('class'));
          var parentLevel = $(parentPositionId).attr('class').split(' ');
          parentLevel.shift();
          parentLevel = parentLevel[0].split('l');
          parentLevel.shift();
          var currLevel = parseInt(parentLevel[0]) + 1;
          var currLevelText = "l" + currLevel;
          console.log(currLevel);

          var rightSeparator = (rightVal !== '') ? ":" : "";
          var style = '';
          var leftSeparator = '<span class="leftSeparator"></span>';

          var nestHtml = '<div class="proofTreeNest ' + currLevelText + '">' +
            '<div class="proofTreeAddSubproof">' +
              '<a href="#" currLevel="' + currLevel + '" class="addSubproof">[+]</a>' +
              '<span class="proofTreeSideConditionLink">' +
                ' | <a href="#" class="addSideCondition">[sc]</a>' +
              '</span>' +
            '</div>' +
            '<div class="proofTreeActions">' +
              '<a href="#" currLevel="' + currLevel + '" class="edit">[e]</a> | ' +
              '<a href="#" class="delete">[x]</a>' +
            '</div>' +
            '<input type="text" class="proofTreeSideCondition" placeholder="Side Condition" />' +
            '<div class="proofTreeLabel">' +
              '<select class="proofTreeDropdownLabel">' +
                '<option selected></option>' +
          '<option name="ident">Ident</option>' +
          '<option name="num">Num</option>' +
          '<option name="bool">BoolConst</option>' +
          '<option name="arith">BinOp</option>' +
          '<option name="and">And</option>' +
          '<option name="or">Or</option>' +
          '<option name="not">Not</option>' +
          '<option name="skip">Skip</option>' +
          '<option name="assign">Assign</option>' +
          '<option name="seq">Seq</option>' +
          '<option name="if">If</option>' +
          '<option name="while">While</option>' +
          '<option name="rel">Rel</option>' +
              '</select>' +
            '</div>' +
            '<div class="proofTreeLine" ' + style + '>' +
              '<span class="proofTreeLineLeft tt">' +
                leftVal +
              '</span><br />' +
                leftSeparator +
              '<br /><span class="proofTreeLineRight tt">' +
                middleVal +
              '</span>' +
              '<span class="proofTreeLineConditionPhrase">' +
                rightSeparator +
              '</span><br />' +
              '<span class="proofTreeLineCondition tt">' +
                rightVal +
              '</span>' +
            '</div>' +
         '</div>';

          // Add the new subtree into the proof tree
          $(parentPositionId).children('.proofTreeAddSubproof').before(nestHtml);

          // Do the resizing and add the corresponding leftSeparator, rerun MathJax
          giveWidths($('#proofTreeContainer').children(), 'root', 0);
          loadLeftSeparator();
          MathJax.Hub.Queue(["Typeset",MathJax.Hub]);

        } // close conditional for root

        $(currentPositionId + ' > .proofTreeLabel > select').val(labelVal);
        $(currentPositionId + ' > .proofTreeSideCondition').val(sideConditionVal);
        if (sideConditionVal != '') {
          $(currentPositionId + ' > .proofTreeSideCondition').css('display', 'inline');
        }
        console.log("label", labelVal);
      }
    }
  }
