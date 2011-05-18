<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<HTML xmlns="http://www.w3.org/1999/xhtml" xml:lang="<?php print $language->language ?>">
  <HEAD><META http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <META http-equiv="X-UA-Compatible" content="IE=EmulateIE7">

    <TITLE><?php print $head_title." - ".$title.$node->title ?></TITLE>
    <META http-equiv="content-language" content="<?php print $language->language ?>">
    <META http-equiv="imagetoolbar" content="no">
    <META name="robots" content="index, follow">
    <?php print $styles ?>
    <!--[if lte IE 6]>
      <link href="/<?php print path_to_theme(); ?>/style/fraunhofer-ie6.css" rel="stylesheet" type="text/css" media="screen, projection, print" />
    <![endif]-->
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
        </DIV>
      </DIV><!-- #header -->
      <!-- HEADER include end -->


      <!-- Main content section -->
      <DIV id="doc">

        <!-- Login Form -->
        <?php
          global $user;
          if ($left && !($user->uid)) {?> <DIV id="contentboxes"><?php print $left ?></DIV>
        <?php } ?>


        <?php
          if ($content != "") { ?>

          <!-- Text content -->
          <DIV id="main" role="main">

            <DIV role="section">
              <DIV class="headings">
                <?php if ($title != "") { ?><h2 class="content-title"><?php print $title ?></h2><?php } else { ?>
                <?php if ($node->title != ""): ?><h2 class="content-title"><?php print $node->title ?></h2><?php endif; } ?>
              </DIV>
            </DIV>

            <DIV class="section" role="section">

              <?php if ($tabs != ""): ?><?php print $tabs ?><?php endif; ?>

              <?php if ($help != ""): ?><p id="help"><?php print $help ?></p><?php endif; ?>
              <?php if ($messages != ""): ?><DIV id="message"><?php print $messages ?></DIV><?php endif; ?>

              <!-- start main content -->
              <?php print($content) ?>
              <!-- end main content -->
            </DIV>

          </DIV><!-- #main -->
        <?php } ?>

        <!-- Menu -->
        <?php
          global $user;
          if ($left && $user->uid) {?> <DIV id="contentboxes"><?php print $left ?></DIV>
        <?php } ?>


        <!-- NAVIGATION include start -->
        <DIV id="nav" class="nav-fhg">
          <H2>Menü</H2>
          <H3>Hauptmenü</H3>
          <!-- php if (isset($primary_links)) { -->
          <?php if (isset($primary_links)) { ?>
            <?php unset($primary_links['menu-617'])  ?> <!--TODO: adapt forum to the mobile theme -->
            <div id="primary">
              <?php print theme('links', $primary_links) ?>
            </div>
          <?php } ?><!-- #nav-direct-first -->
        </DIV>

      </DIV><!-- #doc -->

      <!-- FOOTER include start -->
      <DIV id="footer">
        <DIV class="wrapper">
          <!-- Footer navigation -->

          <ul id="footer-nav" role="contentinfo">
            <li>&copy;2010 Fraunhofer-Gesellschaft</li>
            <li><a href="/kontakt" >Kontakt</a></li>
            <li><a href="/impressum" >Impressum</a></li>
            <li><a href="/content/datenschutzerklärung" >Datenschutzerklärung</a></li>
          </ul>


          <?php print $footer_message;?>

          <!-- Hidden input element to force an update of the screenreader buffer via JavaScript -->
          <FORM method="get" action="http://ve-166.bi.server.de/">
            <FIELDSET>
              <INPUT type="hidden" name="bufferUpdater" id="bufferUpdater" disabled="disabled" value="">
            </FIELDSET>
          </FORM>
        </DIV>
      </DIV><!-- #footer -->

      <!-- Print footer -->
      <DIV id="print-footer">
        <P>
          <STRONG>Quelle: Fraunhofer-Gesellschaft – T2 Übersichts-/Indexseite - Forschungsthemen</STRONG><BR>
          Online im Internet; URL http://ve-166.bi.server.de/international/seitentypen/t2uebersichtseite1<BR>
          [Stand: 26.02.2010, 08:39 Uhr]<BR>
        </P>
      </DIV><!-- #print-footer -->
      <!-- FOOTER include end -->
      
    </DIV><!--page-->
  </BODY>
</HTML>