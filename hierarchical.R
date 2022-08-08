library(DT)
library(data.table)

#Transform dataframe to data.table and turn the dataframe rowname into a data.table column called model
mtcars_dt = data.table(mtcars)
mtcars_dt[["model"]] = rownames(mtcars)
setcolorder(mtcars_dt,c(
  which(colnames(mtcars_dt) %in% c("mpg","cyl","model")),
  which(!colnames(mtcars_dt) %in% c("mpg","cyl","model"))
))

#Turn data table into a nested data.table by mpg, cyl
mtcars_dt <- mtcars_dt[, list(cars=list(.SD)), by = list(mpg,cyl)]

#configure datatable. Hide row number and cars columns [0,4] and enable details control on plus sign column[1]
#turn rows into child rows and remove from parent


datatable(
  cbind(' ' = '&oplus;', mtcars_dt), escape = -2,
  options = list(
    columnDefs = list(
      list(visible = FALSE, targets = c(0,4)),
      list(orderable = FALSE, className = 'details-control', targets = 1)
    )
  ),
  callback = JS("
    table.column(1).nodes().to$().css({cursor: 'pointer'});

    // Format cars object into another table
    var format = function(d) {
      if(d != null){ 
        var result = ('<table id=\"child_' + d[2] + '_' + d[3] + '\">').replace('.','_') + '<thead><tr>'
        for (var col in d[4]){
          result += '<th>' + col + '</th>'
        }
        result += '</tr></thead></table>'
        return result
      }else{
        return '';
      }
    }

    var format_datatable = function(d) {
      var dataset = [];
      for (i = 0; i < + d[4]['model'].length; i++) {
        var datarow = [];
        for (var col in d[4]){
          datarow.push(d[4][col][i])
        }
        dataset.push(datarow)
      }
      var subtable = $(('table#child_' + d[2] + '_' + d[3]).replace('.','_')).DataTable({
        'data': dataset,
        'autoWidth': true, 
        'deferRender': true, 
        'info': false, 
        'lengthChange': false, 
        'ordering': true, 
        'paging': false, 
        'scrollX': false, 
        'scrollY': false, 
        'searching': false 
      });
    };

    table.on('click', 'td.details-control', function() {
      var td = $(this), row = table.row(td.closest('tr'));
      if (row.child.isShown()) {
        row.child.hide();
        td.html('&oplus;');
      } else {
        row.child(format(row.data())).show();
        td.html('&CircleMinus;');
        format_datatable(row.data())
      }
    });"))





