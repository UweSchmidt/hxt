<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:variable name="V" select="."/>

  <xsl:attribute-set name="as">
    <xsl:attribute name="name"><xsl:value-of select="name()"/></xsl:attribute>

  </xsl:attribute-set>

  <xsl:template match="/">
    <result>

      === copy-of of /:
      <xsl:copy-of select = "."/>

      === copy-of of /: with variable V
      <xsl:copy-of select = "$V"/>

      === copy on /:
      <xsl:copy/>
      ...should've been empty

      copy recursively:

      <xsl:apply-templates mode="copy"/> 

    </result>
  </xsl:template>

  <xsl:template match="@*|node()" mode="copy">
    <xsl:copy use-attribute-sets="as">
      <xsl:apply-templates select="node()|@*" mode="copy" />
    </xsl:copy>
  </xsl:template>


</xsl:transform>

