<?xml version="1.0" ?>
<?karl version="1.0" ?>
<!-- comment axg f -->
<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:variable name="V" select="'ascending'"/>

  <xsl:template match="/">
    <result>
      Using for-each:
      <xsl:for-each select = "*/*">
        <xsl:sort select="name()" order="{document/@order}"/>
        <xsl:sort select="@nr" data-type="number"/>
        :<xsl:copy-of select="." />
      </xsl:for-each>
      Using apply-template
      <xsl:apply-templates select="*/*" mode="copy">
        <xsl:sort select="@nr" data-type="number" order="descending"/>
        <xsl:sort select="name()" order="{$V}"/>
        <xsl:with-param name="p" select="concat('value_',document/@order)"/>
      </xsl:apply-templates>
    </result>
  </xsl:template>

  <xsl:template match="*" mode="copy">
    <xsl:param name="p"/>
    Param p: <xsl:value-of select="$p"/>
    <xsl:copy-of select="."/>
  </xsl:template>


</xsl:transform>

