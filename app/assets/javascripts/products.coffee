jQuery ($) ->

  # DOM element and DOM meta-data.
  $table = $('.container table')
  productListUrl = $table.data('list')
  RE_findSingleDigits = /\b(\d)\b/g

  # Load one column of EAN codes
  loadProductTable = ->
    $.get productListUrl, (products) ->
      $.each products, (index, eanCode) ->
        row = $('<tr/>').append $('<td/>').text(eanCode)
        row.attr 'contenteditable', true
        $table.append row
        loadProductDetails row

  # Construct a URL by replacing the EAN code parameter
  productDetailsUrl = (eanCode) ->
    $table.data('details').replace '0', eanCode

  # Load and append product details to a table row.
  loadProductDetails = (tableRow) ->
    eanCode = tableRow.text()
    $.get productDetailsUrl(eanCode), (product) ->
      tableRow.append $('<td/>').text(formatDate(new Date(product.date)))
      tableRow.append $('<td/>').text(Boolean(product.sold))
      tableRow.append $('<td/>').text(product.productname)
      tableRow.append $('<td/>').text(product.userid)
      tableRow.append $('<td/>').text(product.price)
      tableRow.append $('<td/>').text(product.stock)
      tableRow.append $('<td/>').text(product.brand)
      tableRow.append $('<td/>').text(product.category)
      tableRow.append $('<td/>').text(product.condition)
      tableRow.append $('<td/>')

  loadProductTable()


  # Save an edited table row to the server
  saveRow = ($row) ->
    [ean, date, sold, productname, userid, price, stock, brand, category, condition] = $row.children().map -> $(this).text()
    product =
      ean: ean
      date: date
      sold: Boolean(sold)
      productname: productname
      userid: userid
      price: parseFloat(price)
      stock: parseInt(stock)
      brand: brand
      category: category
      condition: condition

    jqxhr = $.ajax
      type: "PUT"
      url: productDetailsUrl(ean)
      contentType: "application/json"
      data: JSON.stringify product
    jqxhr.done (response) ->
      $label = $('<span/>').addClass('label label-success')
      $row.children().last().append $label.text(response)
      $label.delay(3000).fadeOut()
    jqxhr.fail (data) ->
      $label = $('<span/>').addClass('label label-important')
      message = data.responseText || data.statusText
      $row.children().last().append $label.text(message)

  # Attach edit handling to editable elements.
  $table.on 'focusout', 'tr', () ->
    console.log('save')
    saveRow $(this)


  formatDate = (date) ->
    timeStamp = [date.getFullYear(), (date.getMonth() + 1), date.getDate()].join(" ")
    RE_findSingleDigits = /\b(\d)\b/g

    # Places a `0` in front of single digit numbers.
    timeStamp = timeStamp.replace(RE_findSingleDigits, "0$1")
    timeStamp.replace /\s/g, "-"
