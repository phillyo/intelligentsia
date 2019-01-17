function compare() {
  $('#searchAddr1').val('120 Willoughby Ave, Brooklyn');
  $('#searchAddr2').val('21-30 30th Rd, Astoria');
}


$('.form-group input').keypress(function(event){
  if(event.keyCode == 13){
    var addr1 = document.getElementById('addr1').value;
    var addr2 = document.getElementById('addr2').value;
    document.getElementById('searchAddr1').value = addr1;
    document.getElementById('searchAddr2').value = addr2;
    $('li:eq(2) a').tab('show');
    alert(addr1);
  }
});
