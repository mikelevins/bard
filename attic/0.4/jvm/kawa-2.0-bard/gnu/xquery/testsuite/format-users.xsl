<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="users">
<table border="1">
<thead><tr><th>userid</th><th>name</th><th>rating</th></tr></thead>
<xsl:apply-templates/>
</table>
</xsl:template>

<xsl:template match="user_tuple">
<tr><xsl:apply-templates/></tr>
</xsl:template>

<xsl:template match="userid">
<td><xsl:apply-templates/></td>
</xsl:template>

<xsl:template match="name">
<td><xsl:apply-templates/></td>
</xsl:template>

<xsl:template match="rating">
<td><xsl:apply-templates/></td>
</xsl:template>
</xsl:stylesheet>
