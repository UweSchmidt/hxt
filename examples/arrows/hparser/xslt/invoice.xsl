<html xsl:version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <body> 
    <table>
       <xsl:for-each select="*/item">
         <tr>
           <td><xsl:value-of select="@name"/></td>
           <td><xsl:value-of select="@value"/></td>
         </tr>
       </xsl:for-each>
    </table>
  </body>
</html>