<html>
  <head>
    <title>Title</title>
  </head>
  <body>
    <h1>Title</h1>

    <?php

      require "";

      $con = mysqli_connect(DB_host,DB_login,DB_password,DB_database);
      $sql="SELECT * FROM ";
      
      $req = mysqli_query($con,$sql) or die('Error SQL <br/>' .$sql.'<br/>'.mysqli_error($con));

      while($data=mysqli_fetch_assoc($req)){
      }
      mysqli_close($con);

    ?>    

  </body>
</html>
