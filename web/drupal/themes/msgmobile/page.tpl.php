<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<HTML xmlns="http://www.w3.org/1999/xhtml" xml:lang="<?php print $language->language ?>">
  <HEAD><META http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <META http-equiv="X-UA-Compatible" content="IE=EmulateIE7">
    <TITLE><?php print $head_title." - ".$title.$node->title ?></TITLE>
    <META http-equiv="content-language" content="<?php print $language->language ?>">
    <META http-equiv="imagetoolbar" content="no">
    <META name="robots" content="index, follow">
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
          <H1 id="logo"><img src="<?php print $logo ?>" alt="mySmartGrid" /></H1>
          
          <H1 id="quickmenu">
            <?php
            global $_SERVER;
            global $user;
            
            if ($_SERVER['REQUEST_URI'] != '/') { ?>
              <A href="/">Menü</A>&nbsp;&nbsp;&nbsp;
            <?php } ?>
            <?php 
            if ($user->uid) { ?>
              <A href="/logout">Abmelden</A>&nbsp;
            <?php } ?>
          </H1>
        </DIV>
      </DIV><!-- #header -->
      <!-- HEADER include end -->

      <!-- Main content section -->
      <DIV id="doc">
        <!-- Login Form -->
        <?php if (!$user->uid && $_SERVER['REQUEST_URI'] == '/' && $left) {?>
            <DIV id="contentboxes"><?php print $left ?></DIV>
        <?php } ?>

        <!-- Text content -->
        <DIV id="main" role="main">

          <?php if ($title != "" || $node->title != "") { ?>
            <DIV role="section">
              <DIV class="headings">
                <?php if ($title != "") { ?><h2 class="content-title"><?php print $title ?></h2><?php } else { ?>
                <?php if ($node->title != ""): ?><h2 class="content-title"><?php print $node->title ?></h2><?php endif; } ?>
              </DIV>
            </DIV>
          <?php } ?>

          <DIV class="section" role="section">
            <?php if ($tabs != ""): ?><?php print $tabs ?><?php endif; ?>
            <?php if ($help != ""): ?><p id="help"><?php print $help ?></p><?php endif; ?>
            <?php if ($messages != ""): ?><DIV id="message"><?php print $messages ?></DIV><?php endif; ?>

            <!-- start main content -->
            <?php print($content) ?>
            <!-- end main content -->
          </DIV>
        </DIV><!-- #main -->
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
        <P><STRONG>Quelle: Fraunhofer-Gesellschaft – T2 Übersichts-/Indexseite - Forschungsthemen</STRONG><BR>
        Online im Internet; URL http://ve-166.bi.server.de/international/seitentypen/t2uebersichtseite1<BR>
        [Stand: 26.02.2011, 08:39 Uhr]<BR></P>
      </DIV><!-- #print-footer -->
      <!-- FOOTER include end -->
      
    </DIV><!--page-->
  </BODY>
</HTML>