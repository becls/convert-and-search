// Copyright 2018 Beckman Coulter, Inc.
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

function onLoad() {
  $('#offsetInput').val('');
  updateButton();

  $('#offsetInput').keypress(filterKeys);
  $('#offsetInput').keyup(updateButton);
  $('#rowForm').submit(rowToOffset);
}

//Allow 'Enter' key for form submission and digits 0-9, but discard other keys
//Used for the offest text box
function filterKeys(event) {
  if(event.which == 13) {

    return;
  } else if(event.which < 48 || event.which > 57) {

    event.preventDefault();
  }
}

function updateButton() {
  if($('#offsetInput').val()) {

    $('#offsetButton').removeAttr('disabled');
  } else {

    $('#offsetButton').attr('disabled', 'true');
  }
}

function rowToOffset() {
  //Prevent going back to offset 0 when there is no input row
  var row = $('#offsetInput').val();
  if(!row) {

    event.preventDefault();
    return;
  }

  var offset = Math.max(0, row - 1);
  $('#offsetInput').val(offset);
  return true;
}

function initialupdate(){
  hideAll();
  updateCont('div.container', 'column', 'table');
  updateCont('div.excCont', 'exec', 'table');
  updateCont('div.order-contain', 'order', 'table');
  
  updateVal('div.container', 'column', 'table');
  updateVal('div.order-contain', 'order', 'table');
  updateVal('div.excCont', 'exec', 'table');  
}

function joinInitialupdate(){
  hideAll();
  updateJoin1();
  updateJoin2();
  updateOtherFieldJ1();
  updateOtherFieldJ2();
}

function hideAll(){
  $('div.container').children().hide();
  $('div.excCont').children().hide();
  $('div.order-contain').children().hide();
  $('div.contJ2').children().hide();
  $('div.contJ1').children().hide();
}

function updateCont(container, elmId, match){
  var e = document.getElementById(match);
  var strUser = e.options[e.selectedIndex].text;
  var elements = $(container).children().hide();
  document.getElementById(elmId).value = '';
  elements.filter('.' + strUser).show();
}

function updateVal(container, elmId, match){
  var e = document.getElementById(match);
  var strUser = e.options[e.selectedIndex].text;
  var elements = $(container).children();
  var selected = elements.filter('.' + strUser);
  var selectElm = selected[0].children[0];
  var val = selectElm.options[selectElm.selectedIndex].text;
  document.getElementById(elmId).value = val;
}

function updateDrops(){
  updateCont('div.container', 'column', 'table');
  updateCont('div.order-contain', 'order', 'table');
  updateCont('div.excCont', 'exec', 'table');
}

function updateColVal(){
  updateVal('div.container', 'column', 'table');
}

function updateExecVal(){
  updateVal('div.excCont', 'exec', 'table');
}

function updateOrderVal(){
  updateVal('div.order-contain', 'order', 'table');
}



function updateJoin1(){
  updateCont('div.contJ1', 'join1Val', 't1');
}

function updateJoin2(){
  updateCont('div.contJ2', 'join2Val', 't2');
}

function updateOtherFieldJ1(){
  updateVal('div.contJ1', 'join1Val', 't1');
}

function updateOtherFieldJ2(){
  updateVal('div.contJ2', 'join2Val', 't2');
}


$(document).ready(onLoad);
