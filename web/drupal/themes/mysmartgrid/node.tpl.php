<?php

/**
 * The node rendering logic for mySmartgrid.
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
 * Copyright (c) 2010 flukso.net
 *               2010 Fraunhofer Institut ITWM (www.itwm.fraunhofer.de)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
?>
<div id="node-<?php print $node->nid; ?>" class="<?php echo $mysmartgrid_node_class; ?>">

  <?php if ($page == 0): ?>
    <div class="node-headline clearfix">
      <h3><a href="<?php print $node_url; ?>" rel="bookmark" title="<?php print $mysmartgrid_perma_title; ?>"><?php print $title; ?></a></h3>
      <?php if (isset($mysmartgrid_node_timestamp)): ?>
          <span class="timestamp"><?php print $mysmartgrid_node_timestamp; ?></span>
      <?php endif; ?>
      <?php if (isset($mysmartgrid_node_author)): ?>
        <span class="node-author"> – <?php print $mysmartgrid_node_author; ?></span>
      <?php else: ?>
        <span class="node-author">'</span>
      <?php endif; ?>
    </div>
  <?php endif; ?>

  <div class="content clearfix">
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