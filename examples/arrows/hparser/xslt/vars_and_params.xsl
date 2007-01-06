<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:import href="vars_and_params_imp.xsl" />

<xsl:variable name="A" select="concat('global var A refers to:',$C)" />
<xsl:param    name="B" select="concat('global par B',*/*/@nr)" />
<xsl:variable name="C" select="concat('global var C refers to:',$B)" />

<xsl:template match="/">
  <template_match_root>

     Value of variable A:<xsl:value-of select="$A"/>:
     Value of variable B:<xsl:value-of select="$B"/>:
     Value of variable C:<xsl:value-of select="$C"/>:

     Introducing local variable B...
     <xsl:variable name="B" select="'local B'"/>
     Value of variable A:<xsl:value-of select="$A"/>:
     Value of variable B:<xsl:value-of select="$B"/>:
     Value of variable C:<xsl:value-of select="$C"/>:

     Setting variable C to default
     <xsl:variable name="C"/>
     Value of variable A:<xsl:value-of select="$A"/>:
     Value of variable B:<xsl:value-of select="$B"/>:
     Value of variable C:<xsl:value-of select="$C"/>:     

     <xsl:apply-templates select="document">
         <xsl:with-param name="P1"/>
         <xsl:with-param name="P2" select="'passed argument 2'"/>
         <xsl:with-param name="Punknown" select="'unreachable'"/>
     </xsl:apply-templates>
  </template_match_root>
</xsl:template>

    <xsl:template match="document">
       <xsl:param name="P1" select="'default for P1'" />
       <xsl:param name="P2" select="'you will never see me'"/>
       <xsl:param name="P3" select="concat('current-name:',name())"/>
       <xsl:param name="P4" select="concat('P4 depends on P2 :',$P2,': but not the other way around')"/>

       <template_match_document>           

           Value of variable A:<xsl:value-of select="$A"/>:
           Value of variable B:<xsl:value-of select="$B"/>:
           Value of variable C:<xsl:value-of select="$C"/>:

           Value of parameter P1:<xsl:value-of select="$P1"/>:
           Value of parameter P2:<xsl:value-of select="$P2"/>:
           Value of parameter p3:<xsl:value-of select="$P3"/>:
           Value of parameter p3:<xsl:value-of select="$P4"/>:

           <xsl:apply-imports/>

       </template_match_document>
    </xsl:template>

                  <xsl:template name="function">
                      <xsl:param name="depth" select="0"/>
                      <xsl:element name="function_{$depth}">
                      <xsl:if test="$depth &gt; 0">
                         <xsl:call-template name="function">
                            <xsl:with-param name="depth" select="$depth - 1"/>
                         </xsl:call-template>              
                      </xsl:if>
                      </xsl:element>
                  </xsl:template>

</xsl:transform>
