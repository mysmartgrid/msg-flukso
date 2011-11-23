<?php

/**
 * Core functions for the mySmartGrid theme.
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

/**
 * Add a prefix to the terms list and insert a separateor between them.
 *
 * @param $terms The pre-rendered HTML string containing the term list
 *        elements.
 * @param $prefix The text to show before the list of terms. By default the
 *        output of t('Tags: ') is used.
 * @param $separator The character(s) to place between the terms. By default,
 * 		  the output of t(', ') is used.
 *
 * @return The modified HTML.
 */
function mysmartgrid_separate_terms($terms, $prefix = NULL, $separator = NULL) {

  $prefix    = ($prefix == NULL) ? t('Tags: ') : $prefix;
  $separator = ($separator == NULL) ? t(', ') : $separator;
  $output    = $prefix . preg_replace('!a></li>\s<li!',
                                      "a>{$separator}</li>\n<li", $terms);
  return $output;
}

/**
 * Insert a separator between items in the list of links for a node.
 *
 * @param $links The pre-rendered HTML string containing the link list
 *        elements.
 * @param $separator character(s) to place between the links. By default, the
 *        output of t(' | ') is used.
 *
 * @return The modified HTML.
 */
function mysmartgrid_separate_links($links, $separator = ' | ') {

  $separator = ($separator == NULL) ? t(' | ') : $separator;
  $output    = preg_replace('!a></li>\s<li!',
                            "a>{$separator}</li>\n<li", $links);
  return $output;
}

/**
 * Preprocess the nodes.
 *
 * @param &$vars The template variables array. After invoking this function,
 *        these keys will be added to $vars:
 *        - 'mysmartgrid_node_author' - The node's "posted by" text and author
 *          link.
 *        - 'mysmartgrid_node_class' - The CSS classes to apply to the node.
 *        - 'mysmartgrid_node_links' - The node links with a separator placed
 *          between each.
 *        - 'mysmartgrid_perma_title' - The localized permalink text for the node.
 *        - 'mysmartgrid_node_timestamp' - The timestamp for this type, if one
 *          should be rendered for this type.
 *        - 'mysmartgrid_term_links' - The taxonomy links with a separator placed
 *          between each.
 */
function mysmartgrid_preprocess_node(&$vars) {

  $node = $vars['node'];
  
  $vars['mysmartgrid_node_class']  =
    'node ' . ($node->sticky ? 'sticky ' : '') .
    ($node->status ? '' : ' node-unpublished') .
    ' node-' . $node->type .
    ($teaser ? ' teaser' : '') . ' clearfix';

  $vars['mysmartgrid_term_links']  = mysmartgrid_separate_terms($vars['terms']);
  $vars['mysmartgrid_node_links']  = mysmartgrid_separate_links($vars['links']);
  $vars['mysmartgrid_perma_title'] = t('Permanent Link to !title', array('!title' => $vars['title']));

  //Node authorship.
  if (!empty($vars['submitted'])) {
    $vars['mysmartgrid_node_author'] = t('Posted by !author', array('!author' => $vars['name']));
  }

  //Timestamp for this type?
  if (!empty($vars['submitted']) && isset($node->created)) {
    $vars['mysmartgrid_node_timestamp'] = format_date($node->created, 'custom', t('d M Y'));
  }
}

/**
 * Preprocess the pages.
 *
 * @param &$vars The template variables array. After invoking this function,
 *        no page title will be displayed on /node/x pages.
 */
function mysmartgrid_preprocess_page(&$vars) {

  if (substr($_GET['q'], 0, 4) == 'node') {
    $vars['title'] = ''; 
  }

 // -- tab text sould always be Flukso
 $vars['head_title'] = 'mySmartGrid';
}
