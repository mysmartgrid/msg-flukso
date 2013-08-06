<?php
?>
<div id="undocked" role="undocked">
  <?php if ($tabs != ""): ?><?php print render($tabs); ?><?php endif; ?>
  <?php if ($messages != ""): ?><div id="message"><?php print $messages ?></div><?php endif; ?>
  <?php if ($noscript): ?><noscript><div class="messages error"><?php print $noscript; ?></div></noscript><?php endif; ?>
  <div width="100%" height="100%">
    <?php print render($page['content']); ?>
  </div>
</div>
