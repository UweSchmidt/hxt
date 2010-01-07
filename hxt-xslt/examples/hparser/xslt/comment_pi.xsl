<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <!-- XALAN will insert an extra newline after the processing instruction. Therefore the output-files differ. -->
  <xsl:template match="/">
  <result> 
    <xsl:comment>simple-comment</xsl:comment>
    <xsl:comment>-c=a-b  -------------</xsl:comment>
    <xsl:processing-instruction name="secret_instruction_{1+1}">pi-content?></xsl:processing-instruction>
  </result> 
  </xsl:template>
</xsl:transform>