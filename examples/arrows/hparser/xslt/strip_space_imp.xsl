<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:strip-space elements="spacy-elem spacy-elem2" />

<xsl:template match="/">
  <Element xml:space="preserve" >
      <Nested1>

          
      </Nested1>
      <Nested2 xml:space="default">


           <xsl:text>    </xsl:text>

      </Nested2>
      <!-- There is an error in _XALAN_ on the following line. "space" will be treated as "xml:space" which is wrong!
  
           <Nested3 space="default">      </Nested3> 
      -->
      <Nested4>       </Nested4>


    <xsl:copy-of select="document" />
  </Element>
</xsl:template>

</xsl:transform>
