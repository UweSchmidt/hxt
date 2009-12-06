<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- Some work-arounds for errors of in the XPath module:
       - you can select text() nodes below an attribute node, which is illegal (, no matter how HXT treats attributes internally)
       - the body of a processing instruction is not selected with ".".
       - FIXED: ns:* matches everything, regardless of namespaces or even prefixes. 
       - FIXED: root is treated as an element (matches *)(FIXED) AND has the name() "/" instead of "" (FIXED), both illegal. 
    -->     


  <xsl:template match="/">
    <result>
      <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.5 /'" /></xsl:call-template>
    </result>
  </xsl:template>

  <xsl:template match="/" priority="0.4">
    <result>
      <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.4 /'" /></xsl:call-template>
    </result>
  </xsl:template>

  <xsl:template match="/document">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.5 a'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="document">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.0'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="document[self::*]">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.5 b'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="*">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'-0.5'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="@*">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'-0.5'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="text()" priority="-0.4">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'-0.4'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'-0.5'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="processing-instruction('Milk')">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.0'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="processing-instruction()">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'-0.5'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="comment()">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'-0.5'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="comment()" priority="-0.6">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'-0.6'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="/document/element_type2/nested_element">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.5 a'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="element_type2/nested_element">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.5 b'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="ns:*" xmlns:ns="http://ns.org">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'-0.25'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="element_type3/@nr">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.5'" /></xsl:call-template>
  </xsl:template>

  <xsl:template match="@nr">
    <xsl:call-template name="printMatch"><xsl:with-param name="prio" select="'0.0'" /></xsl:call-template>
  </xsl:template>  

  <xsl:template name="printMatch">
    <xsl:param name="prio" />
    Match <xsl:choose> 
      <xsl:when test="self::*">element <xsl:value-of select="name()"/></xsl:when>
      <xsl:when test="self::text()">textnode length <xsl:value-of select="string-length(.)"/> "<xsl:value-of select="."/>"</xsl:when>
      <xsl:when test="self::comment()">comment node: <xsl:value-of select="."/></xsl:when>
      <xsl:when test="self::processing-instruction()">pi node: <xsl:value-of select="name()"/></xsl:when>
      <xsl:when test="not(name())">root-node</xsl:when>
      <xsl:otherwise>attribute <xsl:value-of select="name()"/></xsl:otherwise>
    </xsl:choose> with priority <xsl:value-of select="$prio" /> name : |<xsl:value-of select="name()"/>|
    <!-- <xsl:for-each select="@*|@*">attribute: <xsl:value-of select="name()"/> <xsl:text> </xsl:text></xsl:for-each> -->
    <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction()|node()" >  
      <xsl:sort select="name()" />
    </xsl:apply-templates>
  </xsl:template>

</xsl:transform>