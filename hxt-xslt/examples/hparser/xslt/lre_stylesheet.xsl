<literalResult xsl:version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:for-each select="document/*">
    <element name="{name()}" />
  </xsl:for-each>
</literalResult>