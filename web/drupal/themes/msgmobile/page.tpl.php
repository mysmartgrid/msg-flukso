<?php
?>
<div id="page">
  <div id="header">
    <div class="wrapper">
      <a href="<?php print $front_page ?>"><img src="<?php print $logo ?>" id="logo" /></a>
        <h1 id="quickmenu">
          <?php
          global $_SERVER;
          global $user;
          if ($_SERVER['REQUEST_URI'] != '/') { ?>
            <a href="/">Menü</A>&nbsp;&nbsp;&nbsp;
          <?php } ?>
          <?php 
          if ($user->uid) { ?>
            <a href="/user/logout">Abmelden</a>&nbsp;
          <?php } ?>
        </h1>
      <div class="section">
        <?php print render($page['header']); ?>
      </div>
    </div>
  </div>
  <div id="doc">
    <?php if (!$user->uid && $_SERVER['REQUEST_URI'] == '/' && $page['sidebar_first']): ?>
      <div id="contentboxes">
        <?php print render($page['sidebar_first']); ?>
      </div>
    <?php endif; ?>
    <div id="main" role="main">
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
  </div>
  <div id="footer">
    <div class="wrapper">
      <ul id="footer-nav" role="contentinfo">
        <li>&copy;2011 Fraunhofer-Gesellschaft</li>
        <li><a href="/kontakt" >Kontakt</a></li>
        <li><a href="/impressum" >Impressum</a></li>
        <li><a href="/content/datenschutzerklärung" >Datenschutzerklärung</a></li>
      </ul>
    </div>
  </div>
</div>
