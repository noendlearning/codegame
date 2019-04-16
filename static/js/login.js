$(function ($) {
  //弹出登录
  $("#btn1").hover(function () {
    $(this).stop().animate({
      opacity: '1'
    }, 600);
  }, function () {
    $(this).stop().animate({
      opacity: '0.6'
    }, 1000);
  }).on('click', function () {
    $("body").append("<div id='mask'></div>");
    $("#mask").addClass("mask").fadeIn("slow");
    $("#LoginBox").fadeIn("slow");
  });
  //
  //按钮的透明度
  $("#loginbtn").hover(function () {
    $(this).stop().animate({
      opacity: '1'
    }, 600);
  }, function () {
    $(this).stop().animate({
      opacity: '0.8'
    }, 1000);
  });
  //文本框不允许为空---按钮触发
  $("#loginbtn").on('click', function () {
    var txtName = $("#txtName").val();
    var txtPwd = $("#txtPwd").val();
    if (txtName == "" || txtName == undefined || txtName == null) {
      if (txtPwd == "" || txtPwd == undefined || txtPwd == null) {
        $(".warning").css({ display: 'block' });
      }
      else {
        $("#warn").css({ display: 'block' });
        $("#warn2").css({ display: 'none' });
      }
    }
    else {
      if (txtPwd == "" || txtPwd == undefined || txtPwd == null) {
        $("#warn").css({ display: 'none' });
        $(".warn2").css({ display: 'block' });
      }
      else {
        $(".warning").css({ display: 'none' });
      }
    }
  });
  //文本框不允许为空---单个文本触发
  $("#txtName").on('blur', function () {
    var txtName = $("#txtName").val();
    if (txtName == "" || txtName == undefined || txtName == null) {
      $("#warn").css({ display: 'block' });
    }
    else {
      $("#warn").css({ display: 'none' });
    }
  });
  $("#txtName").on('focus', function () {
    $("#warn").css({ display: 'none' });
  });
  //
  $("#txtPwd").on('blur', function () {
    var txtName = $("#txtPwd").val();
    if (txtName == "" || txtName == undefined || txtName == null) {
      $("#warn2").css({ display: 'block' });
    }
    else {
      $("#warn2").css({ display: 'none' });
    }
  });
  $("#txtPwd").on('focus', function () {
    $("#warn2").css({ display: 'none' });
  });
  //关闭
  $(".close_btn").hover(function () { $(this).css({ color: 'black' }) }, function () { $(this).css({ color: '#999' }) }).on('click', function () {
    $("#LoginBox").fadeOut("fast");
    $("#mask").css({ display: 'none' });
  });
});

//验证邮箱
function check(){
  //alert("1234756");
　　var reg = new RegExp("^[a-z0-9]+([._\\-]*[a-z0-9])*@([a-z0-9]+[-a-z0-9]*[a-z0-9]+.){1,63}[a-z0-9]+$"); //正则表达式
　　var obj = document.getElementById("txtName"); //要验证的对象
　　if(obj.value == ""){ //输入不能为空
　　　　alert("输入不能为空!");
　　　　return false;
　　}else if(!reg.test(obj.value)){ //正则验证不通过，格式不对
　　　　alert("验证不通过!");
        obj.value="";
　　　　return false;
　　}else{
　　　　alert("通过！");
　　　　return true;
　　}
}
