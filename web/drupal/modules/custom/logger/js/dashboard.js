
function formatDate(d) {
  return '' +
    (d.getDate()  < 10 ? '0' : '') + d.getDate() + '/' +
    (d.getMonth() <  9 ? '0' : '') + (d.getMonth() + 1) + '/' +
    d.getFullYear();
}

function formatTime(d) {
  return '' +
    (d.getHours()   < 10 ? '0' : '') + d.getHours() + ':' +
    (d.getMinutes() < 10 ? '0' : '') + d.getMinutes();
}

function updateControlForm(dygraph) {

  var form = document.getElementById('logger-control-form');

  var value = Math.round(dygraph.yAxisRange(0)[0]);
  form.elements['yvalue1'].value = value;

  value = Math.round(dygraph.yAxisRange(0)[1]);
  form.elements['yvalue2'].value = value;

  value = new Date(Math.round(dygraph.xAxisRange(0)[0]));
  form.elements['xvalue1date'].value = formatDate(value);
  form.elements['xvalue1time'].value = formatTime(value);

  value = new Date(Math.round(dygraph.xAxisRange(0)[1]));
  form.elements['xvalue2date'].value = formatDate(value);
  form.elements['xvalue2time'].value = formatTime(value);
}
