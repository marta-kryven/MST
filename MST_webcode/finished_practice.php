<!DOCTYPE html>
<html>
<head>
<title>
Experiment</title>
<style>
table, tr, th, td {
    border:1px solid black;
    border-collapse: collapse;
    width:absolute;
    height:absolute;
}
h1 {
    text-align: center;
}

h2{
    text-align: center;
}

#ex1_container { align:center; text-align: center;}

</style>
</head>
<body onload="loadEventHandler()">

<?php

// form parametres
function test_input($data) {
   $data = trim($data);
   $data = stripslashes($data);
   $data = htmlspecialchars($data);
   return $data;
}

// define variables and set to empty values
$dir = "att-test";
$ip=$_SERVER['REMOTE_ADDR'];
$date = date('d/F/Y h:i:s'); // date of the visit that will be formated this way: 29/May/2011 2512:20:03
$browser = $_SERVER['HTTP_USER_AGENT'];
$browser = str_replace(' ', '_', $browser);
$validrequest = 0;
$showEndNext = "false";

if (!is_writable($dir)) {
    echo 'The directory is not writable ' . $dir . '<br>';
}

if ($_SERVER["REQUEST_METHOD"] == "GET") {

        $UID = "dodgyuser";
        $s = $dir . "/" . $UID . ".txt";
        $f = fopen($s, "a") or die("Unable to open file!" . $s);
	fwrite($f, $ip . " ". $date . " " . $browser . " GET request to test_attrib.php \n");
        fclose($f);
        echo "Err: Get request received.<br>";
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

   if (!empty($_POST["showEndNext"])) {
      $showEndNext = test_input($_POST["showEndNext"]);
   }

   $mazeno = test_input($_POST["mazeno"]);
   $mazeid = test_input($_POST["mazeID"]);
   $time = test_input($_POST["time"]);
   $maze = test_input($_POST["name"]);
   $rating = test_input($_POST["rating"]);
}

if ($validrequest == 1) {
  echo "<p  align='left'>Congratulations! You have finished the practice. We will now move on to the actual study. ";
} else {
  echo "<h2>Err: Invalid Request</h2>\n";
}


?>

<script>

var mn = <?php global $mazeno; echo $mazeno; ?>;
var rmn = <?php global $mazeid; echo $mazeid; ?>;
var mf = "<?php global $maze;  echo  $maze; ?>";
var u_id = "<?php global  $UID; echo  $UID; ?>";
var savedtime = "";
var showEnd = "<?php global $showEndNext; echo $showEndNext; ?>";

function generate_table() {
   var f = "<br><form name='frm' action='test_attrib.php'";

   f = f + " method='post' onsubmit='return submitForm()'>" +
        "<fieldset style='border:0'>" +
            "<input type='text' name='rating' hidden>" + 
            "<input type='text' name='mazeno' hidden><input type='text' name='UID' hidden>" +
            "<input type='text' name='name' hidden><input type=text' name='showEndNext' hidden>" +
            "<input type='text' name='time' hidden><input type='text' name='mazeID' hidden>" +
            "<input type='submit' id = 'sub' value='Submit'/>" +
        "</fieldset>" +
      "</form>";
   return f;
}


function loadEventHandler() {
   document.getElementById("ex1_container").innerHTML = generate_table(); 

   var now= new Date(),
   h= now.getHours(),
   m= now.getMinutes(),
   s= now.getSeconds();
   ms = now.getMilliseconds();

   var times = "t(" + h + "," + m + "," + s + "," + ms + ");";
   savedtime += times;
}


function submitForm() {
       var now= new Date(),
       h= now.getHours(),
       m= now.getMinutes(),
       s= now.getSeconds();
       ms = now.getMilliseconds();

       times = "t(" + h + "," + m + "," + s + "," + ms + ");";
       savedtime += times;

       var m = <?php global $mazeno;  echo "$mazeno" ?>;
       var r = <?php global $rating;  echo "$rating" ?>;
       document.forms["frm"]["showEndNext"].value = showEnd;
       document.forms["frm"]["UID"].value = u_id;
       document.forms["frm"]["mazeno"].value = m;
       document.forms["frm"]["mazeID"].value = rmn;
       document.forms["frm"]["time"].value = savedtime;
       document.forms["frm"]["rating"].value = r;
       document.forms["frm"]["name"].value = mf;
       return true;
}

</script>

<div id="ex1_container">
</div>

</body>
</html> 
