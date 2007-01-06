<xsl:transform 
 version="1.0" 
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:a="a.uri"
 xmlns:b="b.uri"
 xmlns:c="c.uri"
 xmlns:d="d.uri"
 xmlns:e="e.uri"
 xmlns:f="f.uri"
 exclude-result-prefixes="a b"
 extension-element-prefixes="c" xml:space="preserve" >

  

  <xsl:template match="*">
    <result exclude-result-prefixes="d" ><!-- Tricky NOT xsl:... -->
      <inner xsl:extension-element-prefixes="e" xsl:exclude-result-prefixes="f" xmlns:g="g.uri"/>      
    </result>
  </xsl:template>

</xsl:transform>