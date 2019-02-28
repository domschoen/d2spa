jQuery.noConflict();


  function exportTableToCSV($table, filename) {

    var $rows = $table.find('tr').not('.countRow'),

      // Temporary delimiter characters unlikely to be typed by keyboard
      // This is to avoid accidentally splitting the actual contents
      tmpColDelim = String.fromCharCode(11), // vertical tab character
      tmpRowDelim = String.fromCharCode(0), // null character

      // actual delimiter characters for CSV format
      colDelim = '","',
      rowDelim = '"\r\n"',

      // Grab text from table into CSV formatted string
      csv = '"' + $rows.map(function(i, row) {
        var $row = jQuery(row);
        var $cols;
        $cols = $row.children('th, td');
        if(typeof $cols === 'undefined'){
            return ""
        }else{
           return $cols.map(function(j, col) {
                         var $col = jQuery(col);
                         var text = "";
                         if($col.find('table').length > 0){
                            text = "";
                            $col.find('table').find('tr').prepend("<td></td>")
                         }else if($col.find('i').filter(".glyphicon-ok").length > 0){
                            text = "✔︎";
                         }else if($col.find('i').filter(".glyphicon-search").length > 0){
                            text = "🔍";
                         }
                         else{
                            text = $col.text();
                         }
                         return text.replace(/"/g, '""'); // escape double quotes

                       }).get().join(tmpColDelim);
        }
      }).get().join(tmpRowDelim)
      .split(tmpRowDelim).join(rowDelim)
      .split(tmpColDelim).join(colDelim) + '"';

    //remove temp columns added in inner tables
    $table.find('table').find('tr').find('td:first').remove()

    // Deliberate 'false', see comment below
    if (false && window.navigator.msSaveBlob) {

      var blob = new Blob([decodeURIComponent(csv)], {
        type: 'text/csv;charset=utf8'
      });

      // Crashes in IE 10, IE 11 and Microsoft Edge
      // See MS Edge Issue #10396033
      // Hence, the deliberate 'false'
      // This is here just for completeness
      // Remove the 'false' at your own risk
      window.navigator.msSaveBlob(blob, filename);

    } else if (window.Blob && window.URL) {
      // HTML5 Blob
      var blob = new Blob([csv], {
        type: 'text/csv;charset=utf-8'
      });
      var csvUrl = URL.createObjectURL(blob);

      jQuery(this)
        .attr({
          'download': filename,
          'href': csvUrl
        });
    } else {
      // Data URI
      var csvData = 'data:application/csv;charset=utf-8,' + encodeURIComponent(csv);

      $(this)
        .attr({
          'download': filename,
          'href': csvData,
          'target': '_blank'
        });
    }
  }



// This must be a hyperlink
  jQuery(document).on("click",".export-to-excel",function(event) {
    // CSV
   // var args = [jQuery(this).closest(".table"), jQuery(this).attr('title') + '.csv'];
    var args = [jQuery(this).closest('div').parent().find(".table:first"), jQuery(this).attr('title') + '.csv'];
    var currentLengthValue = jQuery("select[name$='_length']").find("option:selected").val()
    jQuery( "select[name$='_length']" ).find("option[value='-1']").prop('selected', true);
    jQuery( "select[name$='_length']" ).change()

    exportTableToCSV.apply(this, args);

    jQuery( "select[name$='_length']" ).find("option[value='"+currentLengthValue+"']").prop('selected', true)
    jQuery( "select[name$='_length']" ).change()

    // If CSV, don't do event.preventDefault() or return false
    // We actually need this to be a typical hyperlink
  });


