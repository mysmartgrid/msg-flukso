<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN" "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="<?php print $language->language ?>" dir="<?php print $language->dir ?>"
  <?php print $rdf_namespaces ?>>
<head profile="<?php print $grddl_profile ?>">

  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
  <meta http-equiv="X-UA-Compatible" content="IE=EmulateIE7"/>

  <title><?php print $head_title." - ".$title.$node->title ?></title>
  <meta http-equiv="content-language" content="<?php print $language->language ?>"/>
  <meta http-equiv="imagetoolbar" content="no"/>
  <meta name="robots" content="index, follow"/>
  <?php print $styles ?>
  <script type="text/javascript" src="/<?php print path_to_theme(); ?>/style/flash-replace-1.01.js"></script>
  <?php print $scripts ?>
  <?php print $head ?>
</head>

<body <?php print theme("onload_attribute"); ?>>

  <div id="page">

      <!-- HEADER include start -->
      <div id="header" role="banner">

        <div class="wrapper">

          <!-- Logo with facility name, linked to start page, except on the start page itself! -->
          <h1 id="logo"><a href="/" tabindex="1" title="zur Startseite" rel="start"><img src="<?php print $logo ?>" alt="mySmartGrid" /></a></h1>
          <h2>Service-Navigation</h2>

          <div class="section">
            <?php if ($header != ""): ?><div id="header"><?php print $header ?></div><?php endif; ?>

            <ul id="meta-nav"></ul>
          </div>
        </div>
      </div><!-- #header -->
      <!-- HEADER include end -->
      

      <!-- Main content section -->
      <div id="doc">
        <img src="/<?php print path_to_theme(); ?>/style/bg-vorlage.jpg" id="doc-bg" alt="" width="997" height="400" />

        <!-- Text content -->
        <div id="main" role="main">
          <div id="breadcrumb"><?php print $breadcrumb ?></div>

          <div role="section">
            <div class="headings">
              <?php if ($title != "") { ?><h2 class="content-title"><?php print $title ?></h2><?php } else { ?>
              <?php if ($node->title != ""): ?><h2 class="content-title"><?php print $node->title ?></h2><?php endif; } ?>
            </div>
          </div>

          <div class="section" role="section">

              
            <?php if ($tabs != ""): ?><?php print render($tabs); ?><?php endif; ?>
            
            <?php if ($help != ""): ?><p id="help"><?php render($page['help']); ?></p><?php endif; ?>
            <?php if ($messages != ""): ?><div id="message"><?php print $messages ?></div><?php endif; ?>

            <!-- start main content -->
            <div class="clearfix">
              <?php print render($page['content']); ?>
            </div>
            <!-- end main content -->
          </div>
          
        </div><!-- #main -->

        <!-- NAVIGATION include start -->
        <div id="nav" class="nav-fhg">
          <h2>Menü</h2>
          <h3>Hauptmenü</h3>
          <?php if (isset($main_menu)) { ?>
            <div id="primary">
              <?php print theme('links', $main_menu) ?>
            </div>
          <?php } ?><!-- #nav-direct-first -->
        </div>

        <?php if ($sidebar_first) { ?>
          <div id="contentboxes">
            <?php print $sidebar_first ?>
          </div>
        <?php } ?>

      </div><!-- #doc -->


      <!-- FOOTER include start -->
      <div id="footer">
        <div class="wrapper">

          <!-- Footer navigation -->
          <ul id="footer-nav" role="contentinfo">
            <li>&copy;2011 Fraunhofer-Gesellschaft</li>
            <li><a href="/kontakt" >Kontakt</a></li>
            <li><a href="/impressum" >Impressum</a></li>
            <li><a href="/content/datenschutzerklärung" >Datenschutzerklärung</a></li>
          </ul>

          <!-- Hidden input element to force an update of the screenreader buffer via JavaScript -->
          <form method="get" action="http://ve-166.bi.server.de/">
            <fieldset>
              <input type="hidden" name="bufferUpdater" id="bufferUpdater" disabled="disabled" value=""/>
            </fieldset>
          </form>
        </div>
      </div><!-- #footer -->

      <!-- Print footer -->
      <div id="print-footer">
        <p>
          <strong>Quelle: Fraunhofer-Gesellschaft – T2 Übersichts-/Indexseite - Forschungsthemen</strong><br/>
            Online im Internet; URL http://ve-166.bi.server.de/international/seitentypen/t2uebersichtseite1<br/>
            [Stand: 26.02.2011, 08:39 Uhr]<br/>
        </p>
      </div><!-- #print-footer -->
      <!-- FOOTER include end -->

    </div>
</body>
</html>