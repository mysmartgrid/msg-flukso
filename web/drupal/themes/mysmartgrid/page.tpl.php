<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN" "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">

<HTML xmlns="http://www.w3.org/1999/xhtml" xml:lang="<?php print $language->language ?>" dir="<?php print $language->dir ?>"
  <?php print $rdf_namespaces ?>>
<HEAD profile="<?php print $grddl_profile ?>">

  <META http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
  <META http-equiv="X-UA-Compatible" content="IE=EmulateIE7"/>

  <TITLE><?php print $head_title." - ".$title.$node->title ?></TITLE>
  <META http-equiv="content-language" content="<?php print $language->language ?>"/>
  <META http-equiv="imagetoolbar" content="no"/>
  <META name="robots" content="index, follow"/>
  <?php print $styles ?>
  <SCRIPT type="text/javascript" src="/<?php print path_to_theme(); ?>/style/flash-replace-1.01.js"></SCRIPT>
  <?php print $scripts ?>
  <?php print $head ?>
</HEAD>

<BODY <?php print theme("onload_attribute"); ?>>

  <DIV id="page">

      <!-- HEADER include start -->
      <DIV id="header" role="banner">

        <DIV class="wrapper">

          <!-- Logo with facility name, linked to start page, except on the start page itself! -->
          <H1 id="logo"><A href="/" tabindex="1" title="zur Startseite" rel="start"><img src="<?php print $logo ?>" alt="mySmartGrid" /></A></H1>
          <H2>Service-Navigation</H2>

          <DIV class="section">
            <?php if ($header != ""): ?><div id="header"><?php print $header ?></div><?php endif; ?>

            <ul id="meta-nav"></ul>
          </DIV>
        </DIV>
      </DIV><!-- #header -->
      <!-- HEADER include end -->
      

      <!-- Main content section -->
      <DIV id="doc">
        <IMG src="/<?php print path_to_theme(); ?>/style/bg-vorlage.jpg" id="doc-bg" alt="" width="997" height="400" />

        <!-- Text content -->
        <DIV id="main" role="main">
          <DIV id="breadcrumb"><?php print $breadcrumb ?></DIV>

          <DIV role="section">
            <DIV class="headings">
              <?php if ($title != "") { ?><h2 class="content-title"><?php print $title ?></h2><?php } else { ?>
              <?php if ($node->title != ""): ?><h2 class="content-title"><?php print $node->title ?></h2><?php endif; } ?>
            </DIV>
          </DIV>

          <DIV class="section" role="section">

              
            <?php if ($tabs != ""): ?><?php print render($tabs); ?><?php endif; ?>
            
            <?php if ($help != ""): ?><p id="help"><?php render($page['help']); ?></p><?php endif; ?>
            <?php if ($messages != ""): ?><DIV id="message"><?php print $messages ?></DIV><?php endif; ?>

            <!-- start main content -->
            <DIV class="clearfix">
              <?php print render($page['content']); ?>
            </DIV>
            <!-- end main content -->
          </DIV>
          
        </DIV><!-- #main -->

        <!-- NAVIGATION include start -->
        <DIV id="nav" class="nav-fhg">
          <H2>Menü</H2>
          <H3>Hauptmenü</H3>
          <?php if (isset($main_menu)) { ?><DIV id="primary"><?php print theme('links', $main_menu) ?></DIV><?php } ?><!-- #nav-direct-first -->
        </DIV>

        <?php if ($sidebar_first) { ?> <DIV id="contentboxes"><?php print $sidebar_first ?></DIV> <?php } ?>

      </DIV><!-- #doc -->


      <!-- FOOTER include start -->
      <DIV id="footer">
        <DIV class="wrapper">

          <!-- Footer navigation -->
          <ul id="footer-nav" role="contentinfo">
            <li>&copy;2011 Fraunhofer-Gesellschaft</li>
            <li><a href="/kontakt" >Kontakt</a></li>
            <li><a href="/impressum" >Impressum</a></li>
            <li><a href="/content/datenschutzerklärung" >Datenschutzerklärung</a></li>
          </ul>

          <!-- Hidden input element to force an update of the screenreader buffer via JavaScript -->
          <FORM method="get" action="http://ve-166.bi.server.de/">
            <FIELDSET>
              <INPUT type="hidden" name="bufferUpdater" id="bufferUpdater" disabled="disabled" value=""/>
            </FIELDSET>
          </FORM>
        </DIV>
      </DIV><!-- #footer -->

      <!-- Print footer -->
      <DIV id="print-footer">
        <P>
          <STRONG>Quelle: Fraunhofer-Gesellschaft – T2 Übersichts-/Indexseite - Forschungsthemen</STRONG><BR/>
            Online im Internet; URL http://ve-166.bi.server.de/international/seitentypen/t2uebersichtseite1<BR/>
            [Stand: 26.02.2011, 08:39 Uhr]<BR/>
        </P>
      </DIV><!-- #print-footer -->
      <!-- FOOTER include end -->

    </DIV>
</BODY>
</HTML>