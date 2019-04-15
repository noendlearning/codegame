$(function ($) {
  //弹出登录
  $("#btn2").hover(function () {
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
    $("#SingupBox").fadeIn("slow");
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
    var txtName1 = $("#txtName1").val();
    var txtPwd1 = $("#txtPwd1").val();
    if (txtName1 == "" || txtName1 == undefined || txtName1 == null) {
      if (txtPwd1 == "" || txtPwd1 == undefined || txtPwd1 == null) {
        $(".warning").css({ display: 'block' });
      }
      else {
        $("#warn3").css({ display: 'block' });
        $("#warn4").css({ display: 'none' });
      }
    }
    else {
      if (txtPwd1 == "" || txtPwd1 == undefined || txtPwd1 == null) {
        $("#warn3").css({ display: 'none' });
        $(".warn4").css({ display: 'block' });
      }
      else {
        $(".warning").css({ display: 'none' });
      }
    }
  });
  //文本框不允许为空---单个文本触发
  $("#txtName1").on('blur', function () {
    var txtName1 = $("#txtName1").val();
    if (txtName1 == "" || txtName1 == undefined || txtName1 == null) {
      $("#warn3").css({ display: 'block' });
    }
    else {
      $("#warn3").css({ display: 'none' });
    }
  });
  $("#txtName1").on('focus', function () {
    $("#warn3").css({ display: 'none' });
  });
  //
  $("#txtPwd1").on('blur', function () {
    var txtName1 = $("#txtPwd1").val();
    if (txtName1 == "" || txtName1 == undefined || txtName1 == null) {
      $("#warn4").css({ display: 'block' });
    }
    else {
      $("#warn4").css({ display: 'none' });
    }
  });
  $("#txtPwd1").on('focus', function () {
    $("#warn4").css({ display: 'none' });
  });
  //关闭
  $(".close_btn").hover(function () { $(this).css({ color: 'black' }) }, function () { $(this).css({ color: '#999' }) }).on('click', function () {
    $("#SingupBox").fadeOut("fast");
    $("#mask").css({ display: 'none' });
  });
});
