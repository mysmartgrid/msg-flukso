<div id="block-<?php print $block->module .'-'. $block->delta; ?>" class="contentbox clear-block block block-<?php print $block->module ?>">

<?php if (!empty($block->subject)): ?>
  <h4><?php print $block->subject ?></h4>
<?php endif;?>

  <div class="section"><?php print $block->content ?></div>
</div>
