jQuery.noConflict();
var timeToWait = 8000;

   jQuery(document).bind('DOMNodeInserted', function(event) {
     var $table = jQuery(event.target).find('.nvList');
     if($table.length > 0 && ! jQuery.fn.DataTable.isDataTable( $table ) ){
         setTimeout(function(){
         try {
              $table.DataTable({
                   'pageLength': 10,
                   'lengthMenu': [ [10, 25, 50, 100, -1], [10, 25, 50,100, "All"] ],
                   //'order': [[ 1, 'asc' ]],
                   'destroy':true,
                   //"columnDefs": [ {"targets": 0,"orderable": false}],
                   "dom": '<"top"i>rt<"bottom"flp><"clear">',
                   searching: false,
                   info: false,
                   "pagingType": "numbers",
                   paging : false
                 });
             }catch(err) {
               //ignore error
             }
             timeToWait = 8000;
           }, timeToWait);
         }
   });



/*
jQuery(document).bind('DOMNodeInserted', function(event) {
     var $table = jQuery(event.target).find('.dummyDiv').parent().find('.nvList');
     if($table.length > 0 ){
         setTimeout(function(){
         try {
              $table.DataTable({
                   'pageLength': 10,
                   'lengthMenu': [ [10, 25, 50, 100, -1], [10, 25, 50,100, "All"] ],
                   //'order': [[ 1, 'asc' ]],
                   'destroy':true,
                   //"columnDefs": [ {"targets": 0,"orderable": false}],
                   "dom": '<"top"i>rt<"bottom"flp><"clear">',
                   searching: false,
                   info: false,
                   "pagingType": "numbers",
                   paging : false
                 });
             }catch(err) {
               //ignore error
             }
             timeToWait = 3000;
           }, 100);
         }
   });

   */

