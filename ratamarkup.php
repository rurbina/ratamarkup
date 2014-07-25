<?php
namespace Ratamarkup;

/*

=Ratamarkup

Procesador de texto orientado a convertir texto plano en HTML correcto. Consta
de dos partes: un procesador de bloques y una colección de procesadores por
bloque llamados simplemente bloques.

==Procesador de bloques
Toma un cuerpo de texto y lo divide buscando líneas que contengan un marcador
de sección (por defecto es §).

==Bloque
Cada bloque recibe una serie de parámetros y un cuerpo de texto, y se espera
que devuelva texto en HTML.

===Bloque normal
El bloque normal es el que contiene el afamado Ratamarkup, que se divide en
dos partes:

 * Proceso de párrafos según el caracter inicial de la línea
 * Proceso de formatos de caracter, tomando algunos estándares de otros wikis.

===Otros bloques
Basta con definir una función markup_blockname dentro del espacio de nombre
de Ratamarkup.

*/

/*
parameters:
data		- a body of text
divclass	- if not null, output will be enclosed in a div with the given class
linkCallback	- if not null, will be called to learn about a link

This function will find lines starting with §. Whenever one is found, it will
be tokenized on whitespace. The first token is a block's name. 

The rest of the tokens will be passed to a function called "block_$tokens[0]",
along with the lines up to the next §.

As a note, unix line endings are expected and enforced.

*/
function process($data, $divclass=null, $options = array()) {

  $data = preg_replace('/\r/','',$data);

  $lines = explode("\n",$data);

  $current_name = 'normal';
  $current_tokens = array('normal');
  $accumulator = '';

  $output = '';

  foreach ($lines as $line) {

    if ( preg_match('/^§/',$line) ) {

      $line = preg_replace('/^§\s+/','',$line);
      $tokens = preg_split('/\s+/',$line);

      if ( function_exists('\Ratamarkup\block_'.$current_name) ) {
	$output .= call_user_func('\Ratamarkup\block_'.$current_name, $accumulator, $current_tokens, $options);
      } else {
	$output .= block_normal($accumulator, $current_tokens, $options);
      }

      $current_name = preg_replace('/^./u', '', $tokens[0]);
      $current_tokens = $tokens;
      $accumulator = '';

    }
    else {
      $accumulator .= "$line\n";
    }

  }

  if ( $accumulator != '' ) {

      if ( function_exists('\Ratamarkup\block_'.$current_name) ) {
	$output .= call_user_func('\Ratamarkup\block_'.$current_name, $accumulator, $current_tokens, $options);
      } else {
	$output .= block_normal($accumulator, $current_tokens, $options);
      }
  }

  if ( $divclass !== null ) {
	  if ( $divclass != "" ) { $class = " class=\"$divclass\""; }
	  $output = "<div{$class}>\n{$output}\n</div>\n\n";
  }

  return $output;

}

function block_normal_list($data,$char,$opt=array()) {

  $sub = '';
  $type = $data[0];
  foreach (explode("\n", $data) as $line) {
    $line = preg_replace("/^[$type][ ]*/", '', $line);
      $sub .= $line."\n\n";
  }
  $processed = block_normal($sub,array(),$opt);

  if ( $type != ':' ) {
    $processed = preg_replace('!(^|\n)<(/?)p>!','$1<$2li>',$processed);

    if ( $type == '#' )
      $processed = "<ol>\n$processed\n</ol>\n";
    else
      $processed = "<ul>\n$processed\n</ul>\n";    
  }
  else {

    $processed = preg_replace('!\n\t!','',$processed);
    $processed = preg_replace('!\n</p>!','</p>',$processed);

    $processed = preg_replace('#(?<=^|\n)<p>(.*?)(?<!\\\\)::(.*?)</p>#','<dt>$1</dt><dd>$2</dd>', $processed);
    $processed = preg_replace('#(?<=^|\n)<p>([^\n]*?)</p>#','<dt>$1</dt>',$processed);
    $processed = preg_replace('#(?<=^|\n)<dt>\s+</dt>#','',$processed);
    $processed = preg_replace('#\\n\\n+#','',$processed);
    $processed = preg_replace('#(?<=^|\n)<#',"\t<",$processed);

    $processed = "<dl>\n$processed\n</dl>\n";
  }

  return $processed; 
}

function block_normal_table($data,$char,$opt=array()) {

	$processed = "<table>\n";

	foreach ( explode("\n", $data) as $row ) {
		if ( $row == "" ) continue;
		$row = substr($row,1);

		$processed .= "\t<tr>\n";
		foreach ( preg_split('/(?<!\\\\)\|/',$row) as $cell ) {
			$td = "td";
			if ( $cell[0] == '=' || $cell[0] == '*' ) {
				$td = 'th';
				$cell = substr($cell,1);
			}
			$processed .= "\t\t<{$td}>";
			$cell = preg_replace('/\\\\\|/','|',$cell);
			$processed .= $opt['no_char'] ? $cell : character_normal($cell,$opt);
			$processed .= "</{$td}>\n";
		}
		$processed .= "\t</tr>\n";
	}

	$processed .= "</table>\n";
	return $processed; 

}

function block_normal($data, $arg, $opt = array()) {

  $blocks = array(
		  '#' => array(
			       'marker' => '#',
			       'name' => 'orderedlist',
			       'process' => function ($data,$opt) { return block_normal_list($data,$opt); },
			       ),
		  '*' => array(
			       'marker' => '*',
			       'name' => 'unorderedlist',
			       'process' => function ($data,$opt) { return block_normal_list($data,$opt); },
			       ),
		  ':' => array(
			       'marker' => ':',
			       'name' => 'deflist',
			       'process' => function ($data,$opt) { return block_normal_list($data,$opt); },
			       ),
		  '' => array(
			      'marker' => '',
			      'name' => 'paragraph',
			      'process' => function ($data,$opt) {
				if ( !strlen($data) ) return "\n";
				$output = "<p>\n";
				foreach ( explode("\n",$data) as $line ) {
				  if ( !strlen($line) ) continue;
				  $output .= "\t".( $opt['no_char'] ? $line : character_normal($line,$opt) )."\n";
				}
				$output .= "</p>\n";
				return $output;
			      }
			      ),
		  '=' => array(
			       'marker' => '=',
			       'name' => 'heading',
			       'process' => function ($data,$opt) {
				 $output = '';
				 foreach (explode("\n",$data) as $line) {
				   if ( !preg_match('/^(=+)(.*)$/',$line,$matches) ) continue;
				   $lvl = strlen($matches[1]) + $opt['header_offset'];
				   if ( $lvl < 1 ) $lvl = 1;
				   if ( $lvl > 6 ) $lvl = 6;
				   $output .= "<h$lvl>".( $opt['no_char'] ? $matches[2] : character_normal($matches[2],$opt) )."</h$lvl>\n";
				 }
				 return $output;
			       }
			       ),
		  '|' => array(
			       'marker' => '|',
			       'name' => 'table',
			       'process' => function ($data,$opt) { return block_normal_table($data,$opt); },
			       ),
		  '!' => array(
			       'marker' => '!',
			       'name' => 'comment',
			       'process' => function ($data,$opt) {
				 $output = '';
				 foreach (explode("\n",$data) as $line) {
				   $line = preg_replace('/^!/','',$line);
				   $output .= $line ? "<!-- $line -->\n" : "\n";
				 }
				 return $output;
			       }
			       ),
		  );
  $valid_markers = array_keys($blocks);

  $current_marker = '';
  $current = $blocks[''];
  $accumulator = '';
  $output = '';

  $lines = explode("\n",$data);

  foreach ( $lines as $line ) {

    if ( strlen($line) > 0 ) {
      $line_marker = $line[0];
    } else {
      $line_marker = 'void';
    }

    if ( array_search($line_marker, $valid_markers) === false && $line_marker != 'void' ) {
      $line_marker = '';
    }

    if ( $line_marker != $current_marker ) {

      if ( is_callable($current['process']) )
	$output .= $current['process']($accumulator,$opt);
      else
	$output .= call_user_func($current['process'], $accumulator,$opt);

      $accumulator = '';
      $current_marker = ($line_marker == 'void') ? '' : $line_marker;
      $current = $blocks[$current_marker];
    }
    
    if ( $line_marker != 'void' )
      $accumulator .= "$line\n";

  }

  if ( $accumulator != '' ) {
    $output .= $current['process']($accumulator,$opt);
  }

  if ( sizeof($arg) > 1 ) {
    $output = "<div class=\"".implode(" ",array_splice($arg,1))."\">\n$output\n</div>\n";
  }

  return $output;

}

function character_normal($line,$opt = array()) {

  if ( ! array_key_exists('link_callback',$opt) ) {
    $link_callback = function ($m) {

      list($src,$href,$text) = $m;
      if ( $text ) {
        return "<a href=\"$href\">$text</a>";
      }
      else {
	return "<a href=\"".str_replace(" ","_",strtolower($href))."\">$href</a>";
      }
    };
  }
  else {
    $link_callback = $opt['link_callback'];
  }

  $pairs = array(
		 '/&/'                                   => '&amp;',
		 '/</'                                   => '&lt;',
		 '/>/'                                   => '&gt;',
		 "/(?<!\\\\)'''''(.*?)(?<!\\\\)'''''/"   => '<b><i>$1</i></b>',
		 "/(?<!\\\\)'''(.*?)(?<!\\\\)'''/"       => '<b>$1</b>',
		 "/(?<!\\\\)''(.*?)(?<!\\\\)''/"         => '<i>$1</i>',
		 '/(?<!\\\\)""(.*?)(?<!\\\\)""/'         => '<tt>$1</tt>',
		 '/(?<!\\\\)__(.*?)(?<!\\\\)__/'         => '<u>$1</u>',
		 '/(?<!\\\\)_-(.*?)(?<!\\\\)-_/'         => '<s>$1</s>',
		 '/(?<!\\\\)\\^\\^(.*?)(?<!\\\\)\\^\\^/' => '<sup>$1</sup>',
		 '/(?<!\\\\)[[]{2}([^<>&|]+?)(?<!\\\\)[]]{2}/'                      => $link_callback,
		 '/(?<!\\\\)[[]{2}([^<>&]+?)(?<!\\\\)[|]([^<>&]+?)(?<!\\\\)[]]{2}/' => $link_callback,
		 '/(?<!\\\\)[{]{2}([^<>&]+?)(?<!\\\\)[|](.*?)(?<!\\\\)[}]{2}/' => '<img src="$1" alt="$2" title="$m2">',
		 '/(?<!\\\\)[{]{2}(.*?)(?<!\\\\)[}]{2}/'                  => '<img src="$1" alt="$1"/>',
		 '/(?<!\\\\)---/'             => '&mdash;',
		 '/(?<!\\\\)--/'              => '&ndash;',
		 '/\\\\([{}\'"_^-]|\\[|\\])/' => '$1',
		 );

  foreach ( $pairs as $pattern => $replace ) {
    
    if ( is_callable($replace) )
      $line = preg_replace_callback($pattern, $replace, $line);
    else
      $line = preg_replace($pattern, $replace, $line);

  }

  return trim($line);

}

