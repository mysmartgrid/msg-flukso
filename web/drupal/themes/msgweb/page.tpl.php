<?php
?>
<div id="page">
  <div id="header">
    <div class="wrapper">
      <a href="<?php print $front_page ?>"><img src="<?php print $logo ?>" id="logo" width="243" height="58"/></a>
      <div class="section"><?php print render($page['header']); ?></div>
    </div>
  </div>
  <div id="doc">
    <img src="<?php print $theme_url; ?>/images/doc-bg.jpg" id="doc-bg" width="997" height="400"/>
    <div id="main" role="main">
      <div id="breadcrumb"><?php print $breadcrumb; ?></div>
      <div role="section">
        <div class="headings">
          <?php if ($title != "") { ?><h2 class="content-title"><?php print $title; ?></h2><?php } else { ?>
          <?php if ($node->title != ""): ?><h2 class="content-title"><?php print $node->title; ?></h2><?php endif; } ?>
        </div>
      </div>
      <div class="section" role="section">
        <?php if ($tabs != ""): ?><?php print render($tabs); ?><?php endif; ?>
        <?php if ($page['help'] != ""): ?><p id="help"><?php render($page['help']); ?></p><?php endif; ?>
        <?php if ($messages != ""): ?><div id="message"><?php print $messages ?></div><?php endif; ?>
        <?php if ($noscript): ?><noscript><div class="messages error"><?php print $noscript; ?></div></noscript><?php endif; ?>
        <div class="clearfix">
          <?php print render($page['content']); ?>
        </div>
      </div>
    </div>
    <div id="nav" class="nav-fhg">
      <?php if (isset($main_menu)) { ?>
        <div id="primary">
          <?php print theme('links', array('links' => $main_menu,
            'attributes' => array('id' => 'main-menu', 'class' => array('links', 'clearfix')),
	    'header' => array('text' => t('Main menu'), 'level' => 'h2', 
	    'class' => array('element-invisible')))); ?>
       </div>
      <?php } ?>
    </div>
    <?php if ($page['sidebar_first']): ?>
      <div id="contentboxes">
        <?php print render($page['sidebar_first']); ?>
      </div>
    <?php endif; ?>
  </div>
  <div id="footer">
    <div class="wrapper">
      <ul id="footer-nav" role="contentinfo">
        <li>&copy; <?php print date("Y"); ?> Fraunhofer-Gesellschaft</li>
        <li><a href="/kontakt" >Kontakt</a></li>
        <li><a href="/impressum" >Impressum</a></li>
        <li><a href="/content/datenschutzerklärung" >Datenschutzerklärung</a></li>
      </ul>
    </div>
  </div>
  <div id="print-footer">
    <p>
      <strong>Quelle: Fraunhofer-Gesellschaft – T2 Übersichts-/Indexseite - Forschungsthemen</strong><br/>
        Online im Internet; URL http://ve-166.bi.server.de/international/seitentypen/t2uebersichtseite1<br/>
        [Stand: 26.02.2011, 08:39 Uhr]<br/>
    </p>
  </div>
</div>
