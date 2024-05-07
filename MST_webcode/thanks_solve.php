<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Thank you!</title>
<style>
#ex2_container { text-align:center; font-size:120%;}
</style>
</head>

<?php
// form parametres
function test_input($data) {
   $data = trim($data);
   $data = stripslashes($data);
   $data = htmlspecialchars($data);
   return $data;
}

$ip=$_SERVER['REMOTE_ADDR'];
$date = date('d/F/Y h:i:s'); // date of the visit that will be formated this way: 29/May/2011 2512:20:03
$browser = $_SERVER['HTTP_USER_AGENT'];
$browser = str_replace(' ', '_', $browser);
$validRequest = 0;
$showEndNext = "true";
// set this to one when comment submitted
$dir = "att-test";
$vallidRequest=0;

// do not allow GET
if ($_SERVER["REQUEST_METHOD"] == "GET") {
     $vallidRequest=0;
     $s = $dir . "/" . $UID . ".txt";        
     $f = fopen($s, "a") or die("Unable to open file!" . $s);
     fwrite($f, $ip . "\t". $date . "\t" . $browser . "\tthabksGETREQUEST\n");
     fclose($f);
     echo "Error: GET request received<br>";
}

if ($_SERVER["REQUEST_METHOD"] == "POST") {
       $validrequest = 1;

       if (!empty($_POST["UID"])) {
         $UID = test_input($_POST["UID"]);
         if ( !strcmp($UID, "dodgyuser") ) {
            $validrequest = 0;
         }
       } else {
         $validrequest = 0;
         $UID = "dodgyuser";
       }


       $mazeno = test_input($_POST["mazeno"]);
       $path = test_input($_POST["path"]);
       $time = test_input($_POST["time"]);
       $maze = test_input($_POST["name"]);
       $UID = test_input($_POST["UID"]);
       $steps = test_input($_POST["steps"]);
       $mazeid = test_input($_POST["mazeID"]);
       $s = $dir . "/" . $UID . ".txt";
       $f = fopen($s, "a") or die("Unable to open file!");

       if (!empty($maze)) {
          fwrite($f, $ip . " ". $date . " " . $browser . " " . $UID . " " . $maze . " " . $steps . " " . $path . " " . $time . "\n");
       } 
       fclose($f);
}

?>

 
<body onload="loadEventHandler()">
<div id="ex2_container">
<br><br><br>
<p style='font-family:Optima'> Thank you! </p>
<br><br>
<form name="frm" action="int_exp2.php" method="post" onsubmit="return validateForm()">
  <p style='font-family:Optima'> How did you make your decisions about which way to go? </p> <br>
  <textarea name="decision" rows="5" cols="70"></textarea><br><br>
  <input type="text" name="steps" hidden> <input type="text" name="UID" hidden><input type='text' name='showEndNext' hidden><br>
  <input type="submit" value="Submit" style="padding:10px 20px; background:#ccff00; border:0 none; cursor:pointer; box-shadow: 2px 2px #888888;"/>
</form>
</div>


<script>

function loadEventHandler() {
      document.forms["frm"].action="generalquestions.php";
}

function validateForm() {
        //alert('thanks ' + document.forms["frm"]["decision"].value );
	document.forms["frm"]["UID"].value = "<?php global $UID; echo $UID; ?>";
        document.forms["frm"]["steps"].value = "<?php global $steps; echo $steps; ?>";
        var x = document.forms["frm"]["decision"].value;
        document.forms["frm"]["showEndNext"].value = "true";
        if (x == null || x == "" || x == 0) {
          alert("Please answer the question to proceed." + x);
          return false;
       }

       return true;
}
</script>

</body>
</html>
