<?php
?>

<div id="page">

  <?php print render($page['header']); ?>

  <div id="header">
    <div class="wrapper">
      <a href="<?php print $front_page ?>"><img src="<?php print $logo ?>" id="logo" /></a>

      <div class="section">
        <?php if ($header != ""): ?><div id="header"><?php print $header ?></div><?php endif; ?>
      </div>
    </div>
  </div>


  <div id="doc">
    <img src="/<?php print path_to_theme(); ?>/images/doc-bg.jpg" id="doc-bg" alt="" width="997" height="400" />

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
	    'class' => array('element-invisible'))));  ?>
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
  </div>

  <div id="print-footer">
    <p>
      <strong>Quelle: Fraunhofer-Gesellschaft – T2 Übersichts-/Indexseite - Forschungsthemen</strong><br/>
        Online im Internet; URL http://ve-166.bi.server.de/international/seitentypen/t2uebersichtseite1<br/>
        [Stand: 26.02.2011, 08:39 Uhr]<br/>
    </p>
  </div>
</div>
