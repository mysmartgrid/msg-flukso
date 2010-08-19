<?php
/**
 * @file node.tpl.php
 * The node rendering logic for Flukso.
 *
 * In addition to the standard variables Drupal makes available to node.tpl.php,
 * these variables are made available by the theme:
 *
 * - $mysmartgrid_node_author - The node's "posted by" text and author link.
 *
 * - $mysmartgrid_node_class - The CSS classes to apply to the node.
 *
 * - $mysmartgrid_node_links - The node links with a separator placed between each.
 *
 * - $mysmartgrid_perma_title - The localized permalink text for the node.
 *
 * - $mysmartgrid_term_links - The taxonomy links with a separator placed between
 *   each.
 *
 * - $mysmartgrid_node_timestamp - The timestamp for this type, if one should be
 *   rendered for this type.
 *
 * $Id$
 */
?>
<div id="node-<?php print $node->nid; ?>" class="<?php echo $mysmartgrid_node_class; ?>">

  <div class="node-headline clear-block">
  <?php if ($page == 0): ?>
    <h3><a href="<?php print $node_url; ?>" rel="bookmark" title="<?php print $mysmartgrid_perma_title; ?>"><?php print $title; ?></a></h3>
  <?php endif; ?>
    <?php if (isset($mysmartgrid_node_timestamp)): ?>
        <span class="timestamp"><?php print $mysmartgrid_node_timestamp; ?></span>
    <?php endif; ?>
    <?php if (isset($mysmartgrid_node_author)): ?>
    	<span class="node-author"> – <?php print $mysmartgrid_node_author; ?></span>
    <?php else: ?>
      <span class="node-author">'</span>
    <?php endif; ?>
  </div>

  <div class="content clear-block">
    
    <?php print $picture; ?>
    <?php print $content; ?>
  </div>
  <?php if (!empty($taxonomy) || !empty($links)): ?>
    <div class="meta">
      <?php
      if ($taxonomy) { print '<div class="tags">'.$mysmartgrid_term_links.'</div>'; }
      if (!empty($links)): ?>
          <div class="more"><?php print $mysmartgrid_node_links; ?></div>
      <?php endif; ?>
    </div>
  <?php endif; ?>
</div> <!-- node -->
