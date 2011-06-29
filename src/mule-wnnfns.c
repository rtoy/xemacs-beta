/* -*- coding: utf-8 -*-
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2005, 2010 Ben Wing.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: Mule 2.3.  Not in FSF. */

/*      Jserver Interface for Mule
        Coded by Yutaka Ishikawa at ETL (yisikawa@etl.go.jp)
                 Satoru Tomura   at ETL (tomura@etl.go.jp)
	Modified for Wnn4 library by
		 Toshiaki Shingu (shingu@cpr.canon.co.jp)
		 Hiroshi Kuribayashi (kuri@nff.ncl.omron.co.jp) */

/*
 *	Functions defined in this file are
 *	   (wnn-server-open wnn-host-name login-name)
 *		wnn-host-name: STRING or NIL
 *		login-name: STRING
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		jserver と接続し、サーバー内部に正変換／逆変換２つの環境を
 *		作る。エラーの時は nil を返す。
 *
 *	   (wnn-server-close)
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		jserver との接続を切る。辞書、頻度はセーブされない。
 *
 *	   (wnn-server-dict-add dict-file-name hindo-file-name priority
 *		dict-file-mode hindo-file-mode pw1 pw2)
 *		dict-file-name: STRING
 *		hindo-file-name: STRING or NULL-STRING
 *		priority: INTEGER
 *		dict-file-mode: BOOLEAN
 *		hindo-file-mode: BOOLEAN
 *		pw1: STRING or NIL
 *		pw2: STRING or NIL
 *		DESCRIPTION:
 *		辞書ファイル名、頻度ファイル名、優先度、辞書ファイルモード
 *		頻度ファイルモードで指定した辞書をバッファに追加する。
 *		pw1, pw2 は辞書ファイル、頻度ファイルのパスワード。
 *
 *	   (wnn-server-dict-delete dic-no)
 *		dic-no: INTEGER
 *		RETURNS: エラーの時 nil
 *		DESCRIPTION: dic-no の辞書番号の辞書を、バッファから
 *		削除する。
 *
 *	   (wnn-server-dict-list)
 *		RETURNS: ((dic-no1 file-name1 comment1 word-no1 nice1)
 *			  (dic-no2 file-name2 comment2 word-no2 nice2)...)
 *		DESCRIPTION: バッファ上の辞書のリストを得る。
 *
 *	   (wnn-server-dict-comment dic-no comment)
 *		RETURNS: エラーの時 nil
 *		DESCRIPTION: dic-no の辞書にコメントをつける。
 *
 *	   (wnn-server-set-rev rev)
 *		rev: BOOLEAN
 *		rev が nil の時は正変換、それ以外の時は逆変換
 *
 *	   (wnn-server-henkan-begin henkan-string)
 *		henkan-string: STRING
 *		RETURNS: bunsetu-suu
 *		DESCRIPTION:
 *		仮名漢字変換をし、第一候補の文節数を返す。
 *
 *	   (wnn-server-zenkouho bunsetu-no dai)
 *		bunsetu-no: INTEGER
 *		dai: BOOLEAN
 *		RETURNS: offset
 *		DESCRIPTION:
 *		文節番号で指定された文節の全候補をとりだし
 *		、現在のオフセットを返す。
 *
 *	   (wnn-server-get-zenkouho offset)
 *		bunsetu-no: INTEGER
 *		dai: BOOLEAN
 *		RETURNS: list of zenkouho
 *		DESCRIPTION:
 *		オフセットで指定された候補を得る。
 *
 *	   (wnn-server-zenkouho-bun)
 *		RETURNS: INTEGER
 *		DESCRIPTION:
 *		全候補を表示している文節番号を得る。
 *
 *	   (wnn-server-zenkouho-suu)
 *		RETURNS: INTEGER
 *		DESCRIPTION:
 *		全候補を表示している文節の全候補数を得る。
 *
 *	   (wnn-server-dai-top bun-no)
 *		bun-no: INTEGER
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		文節が大文節の先頭なら t
 *
 *	   (wnn-server-dai-end bun-no)
 *		bun-no: INTEGER
 *		RETURNS: INTEGER
 *		DESCRIPTION:
 *		次の大文節の文節番号を得る。
 *
 *	   (wnn-server-henkan-kakutei kouho-no dai)
 *		kouho-no: INTEGER
 *		dai: BOOLEAN
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		候補番号で示された候補を選択する。
 *		(wnn-server-zenkouho) を呼んてからでないといけない。
 *
 *	   (wnn-server-bunsetu-henkou bunsetu-no bunsetu-length dai)
 *		bunsetu-no: INTEGER
 *		bunsetu-length: INTEGER
 *		dai: BOOLEAN
 *		RETURNS:
 *		DESCRIPTION:
 *		文節の長さを変更する。
 *
 *         (wnn-bunsetu-kouho-inspect bunsetu-no)
 *              bunsetu-no: INTEGER
 *              RETURNS: (kanji yomi jisho-no serial-no hinsi hindo
 *		ima hyoka daihyoka kangovect)
 *		DESCRIPTION:
 *		文節の色々な情報を変換バッファからとり出す。
 *
 *	   (wnn-server-henkan-quit)
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		何もしない。
 *
 *	   (wnn-server-bunsetu-kanji bun-no)
 *		RETURNS: (bunsetu-kanji length)
 *		DESCRIPTION:
 *
 *	   (wnn-server-bunsetu-yomi bun-no)
 *		RETURNS: (bunsetu-yomi length)
 *		DESCRIPTION:
 *
 *	   (wnn-server-bunsetu-suu)
 *		RETURNS: bunsetu-suu
 *		DESCRIPTION:
 *
 *	   (wnn-server-hindo-update &optional bunsetu-no)
 *              bunsetu-no: INTEGER
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		頻度情報を更新する。
 *
 *	   (wnn-server-word-add dic-no tango yomi comment hinsi)
 *		dic-no: INTEGER
 *		tango: STRING
 *		yoni: STRING
 *		comment: STRING
 *		hinsi: INTEGER
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		辞書に単語を登録する。
 *
 *	   (wnn-server-word-delete dic-no entry)
 *		dic-no: INTEGER
 *		entry: INTEGER
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		辞書からエントリ番号で示される単語を削除する。
 *
 *	   (wnn-server-word-use dic-no entry)
 *		dic-no: INTEGER
 *		entry: INTEGER
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		辞書からエントリ番号で示される単語の有効／無効をトグルする。
  *
 *	   (wnn-server-word-info dic-no entry)
 *		dic-no: INTEGER
 *		entry: INTEGER
 *		RETURNS: (yomi kanji comment hindo hinsi)
 *		DESCRIPTION:
 *		辞書からエントリ番号で示される単語の情報を得る。
 *
 *	   (wnn-server-word-hindo-set dic-no entry hindo)
 *		dic-no: INTEGER
 *		entry: INTEGER
 *		hindo: INTEGER
 *		RETURNS: BOOLEAN
 *		DESCRIPTION:
 *		辞書からエントリ番号で示される単語の頻度を設定する。
 *
 *	   (wnn-server-word-search yomi)
 *		yomi: STRING
 *		RETURNS: a LIST of dict-joho
 *		DESCRIPTION:
 *		全ての辞書から単語検索を行なう。
 *
 *         (wnn-server-dict-save)
 *              RETURNS: BOOLEAN
 *              DESCRIPTION:
 *		全ての辞書と頻度ファイルをセーブする。
 *
 *	   (wnn-server-get-param)
 *		RETURNS: (n nsho p1 p2 p3 ... p15)
 *		DESCRIPTION: 変換パラメータを得る。
 *
 *	   (wnn-server-set-param n sho p1 ... p15)
 *		RETURNS: エラーの時 nil
 *		DESCRIPTION: 変換パラメータを設定する。
 *
 *	   (wnn-server-get-msg error-no)
 *		RETURNS: エラーメ臆技ージ
 *		DESCRIPTION: エラー番号からメッセージを得る。
 *
 *	   (wnn-server-fuzokugo-set fname)
 *		RETURNS: エラーの時 nil
 *		DESCRIPTION: バッファに附属語ファイルを読み込む。
 *
 *	   (wnn-server-fuzokugo-get)
 *		RETURNS: ファイル名
 *		DESCRIPTION: バッファの附属語ファイル名を得る。
 *
 *	   (wnn-server-isconnect)
 *		RETURNS: コネクトしてれば t, してなければ nil
 *		DESCRIPTION: サーバと継っているか調べる。
 *
 *	   (wnn-server-hinsi-dicts hinsi-no)
 *		RETURNS: (dic-no1 dic-no2 ...)
 *		DESCRIPTION: hinsi-no の品詞が登録できる辞書のリストを得る。
 *		hinsi-no = -1 のときには、登録可能な全辞書を得る。
 *
 *	   (wnn-server-hinsi-list dic-no name)
 *		RETURNS: (name1 name2 ... )
 *		DESCRIPTION: dic-no の辞書で、品詞ノードに属する
 *		品詞ノード（名）のリストを得る。
 *		品詞名を与えた時は、０を返す。
 *
 *	   (wnn-server-hinsi-name hinsi-no)
 *		RETURNS: hinsi-name
 *		DESCRIPTION: 品詞番号から名前を取る。
 *
 *	   (wnn-server-hinsi-number hinsi-name)
 *		RETURNS: hinsi-no
 *		DESCRIPTION: 品詞名を品詞番号に変換する。
 *
 *         (wnn-server-version)
 *              RETURNS: version ID(int)
 *
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "charset.h"
#include "window.h"
#include "sysdep.h"

#include "wnn/commonhd.h"
#include "wnn/jllib.h"
#include "wnn/cplib.h"

/* UCHAR が二重定義されるので */
#define _UCHAR_T

#define EGG_TIMEOUT 5
#define NSERVER 4
#define WNNSERVER_J 0
#define WNNSERVER_C 1
#define WNNSERVER_T 2
#define WNNSERVER_K 3

int check_wnn_server_type (void);
void w2m (w_char *wp, Ibyte *mp, Lisp_Object charset);
void m2w (Ibyte *mp, w_char *wp);
void w2y (w_char *w);
void c2m (UExtbyte *cp, Ibyte *mp, Lisp_Object charset);
static void puts2 (char *s);
static int dai_end (int no, int server);
static int yes_or_no (UExtbyte *s);

 /* Why doesn't wnn have a prototype for these? */
typedef unsigned int letter;
int cwnn_yincod_pzy(w_char *, w_char, int);
int cwnn_pzy_yincod(letter *, letter *, int);

static struct wnn_buf *wnnfns_buf[NSERVER];
static struct wnn_env *wnnfns_env_norm[NSERVER];
static struct wnn_env *wnnfns_env_rev[NSERVER];
static int wnnfns_norm;
static Lisp_Object charset_wnn_server_type[NSERVER];

/* Lisp Variables and Constants Definition */
Lisp_Object	Qjserver;
Lisp_Object	Qcserver;
/*Lisp_Object	Qtserver;*/
Lisp_Object	Qkserver;
Lisp_Object	Qwnn_no_uniq;
Lisp_Object	Qwnn_uniq;
Lisp_Object	Qwnn_uniq_kanji;
Lisp_Object	Qwnn_n, Qwnn_nsho, Qwnn_hindo, Qwnn_len, Qwnn_jiri, Qwnn_flag;
Lisp_Object	Qwnn_jisho, Qwnn_sbn, Qwnn_dbn_len, Qwnn_sbn_cnt, Qwnn_suuji;
Lisp_Object	Qwnn_kana, Qwnn_eisuu, Qwnn_kigou, Qwnn_toji_kakko, Qwnn_fuzokogo, Qwnn_kaikakko;
Lisp_Object	Vwnn_server_type;
Lisp_Object	Vcwnn_zhuyin;
Lisp_Object	Vwnnenv_sticky;
Lisp_Object	Vwnn_uniq_level;
Lisp_Object     Qchinese_sisheng;

/* Lisp functions definition */

DEFUN ("wnn-server-open", Fwnn_open, 2, 2, 0, /*
Connect to jserver of host HNAME, make an environment with
login name LNAME in the server.
Return nil if error occurs.
*/
     (hname, lname))
{
  Extbyte *envname;
  Ascbyte *langname;
  Extbyte *hostname;
  int	snum;

  snum = check_wnn_server_type ();
  switch (snum)
    {
    case WNNSERVER_J:
      langname = "ja_JP";
      break;
    case WNNSERVER_C:
      langname = "zh_CN";
      break;
/*
    case WNNSERVER_T:
    strcpy (langname, "zh_TW");
    break;
    */
    case WNNSERVER_K:
      langname = "ko_KR";
      break;
    case -1:
    default:
      return Qnil;
    }
  /* #### This is extremely stupid.  I'm sure these alloca() copies are
     unnecessary, but the old code went out of its way to do this. --ben */
  CHECK_STRING (lname);
  EXTBYTE_STRING_TO_ALLOCA (LISP_STRING_TO_EXTERNAL (lname, Qnative),
			    envname);
  if (NILP (hname)) hostname = "";
  else
    {
      CHECK_STRING (hname);
      EXTBYTE_STRING_TO_ALLOCA (LISP_STRING_TO_EXTERNAL (hname, Qnative),
				hostname);
    }
  /* 97/4/16 jhod@po.iijnet.or.jp
   * libwnn uses SIGALRM, so we need to stop and start interrupts.
   */
  stop_interrupts ();
  if (!(wnnfns_buf[snum] = jl_open_lang (envname, hostname, langname,
					 0, 0, 0, EGG_TIMEOUT)))
    {
      start_interrupts ();
      return Qnil;
    }
  if (!jl_isconnect (wnnfns_buf[snum]))
    {
      start_interrupts ();
      return Qnil;
    }
  wnnfns_env_norm[snum] = jl_env_get (wnnfns_buf[snum]);
/*  if (Vwnnenv_sticky == Qt) jl_env_sticky_e (wnnfns_env_norm[snum]);
    else jl_env_un_sticky_e (wnnfns_env_norm[snum]);*/
  strcat (envname, "R");
  if (!(wnnfns_env_rev[snum] = jl_connect_lang (envname, hostname, langname,
						0, 0, 0, EGG_TIMEOUT)))
    {
      start_interrupts ();
      return Qnil;
    }
/*  if (Vwnnenv_sticky == Qt) jl_env_sticky_e (wnnfns_env_rev[snum]);
    else jl_env_un_sticky_e (wnnfns_env_rev[snum]);*/
  start_interrupts ();
  return Qt;
}


DEFUN ("wnn-server-close", Fwnn_close, 0, 0, 0, /*
Close the connection to jserver, Dictionary and frequency files
are not saved.
*/
     ())
{
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (wnnfns_env_norm[snum])
    {
      if (NILP (Vwnnenv_sticky)) jl_env_un_sticky_e (wnnfns_env_norm[snum]);
      else jl_env_sticky_e (wnnfns_env_norm[snum]);
      jl_disconnect (wnnfns_env_norm[snum]);
    }
  if (wnnfns_env_rev[snum])
    {
      if (NILP (Vwnnenv_sticky)) jl_env_un_sticky_e (wnnfns_env_rev[snum]);
      else jl_env_sticky_e (wnnfns_env_rev[snum]);
      jl_disconnect (wnnfns_env_rev[snum]);
    }
  jl_env_set (wnnfns_buf[snum], 0);
  jl_close (wnnfns_buf[snum]);
  wnnfns_buf[snum] = (struct wnn_buf *) 0;
  wnnfns_env_norm[snum] = wnnfns_env_rev[snum] = (struct wnn_env *) 0;
  return Qt;
}

DEFUN ("wnn-server-dict-add", Fwnn_dict_add, 5, MANY, 0, /*
Add dictionary specified by DICT-FILE-NAME, FREQ-FILE-NAME,
PRIORITY, DICT-FILE-MODE, FREQ-FILE-MODE.
Specify password files of dictionary and frequency, PW1 and PW2, if needed.
*/
     (int nargs, Lisp_Object *args))
{
  struct gcpro gcpro1;
  int	snum;
  CHECK_STRING (args[0]);
  CHECK_STRING (args[1]);
  CHECK_INT (args[2]);
  if (! NILP (args[5])) CHECK_STRING (args[5]);
  if (! NILP (args[6])) CHECK_STRING (args[6]);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  GCPRO1 (*args);
  gcpro1.nvars = nargs;
  if (jl_dic_add (wnnfns_buf[snum],
		  LISP_STRING_TO_EXTERNAL (args[0], Qfile_name),
		  LISP_STRING_TO_EXTERNAL (args[1], Qfile_name),
		  wnnfns_norm ? WNN_DIC_ADD_NOR : WNN_DIC_ADD_REV,
		  XINT (args[2]),
		  NILP (args[3]) ? WNN_DIC_RDONLY : WNN_DIC_RW,
		  NILP (args[4]) ? WNN_DIC_RDONLY : WNN_DIC_RW,
		  NILP (args[5]) ? 0 :
		  LISP_STRING_TO_EXTERNAL (args[5], Qfile_name),
		  NILP (args[6]) ? 0 :
		  LISP_STRING_TO_EXTERNAL (args[6], Qfile_name),
		  yes_or_no,
		  puts2 ) < 0)
    {
      UNGCPRO;
      return Qnil;
    }
  UNGCPRO;
  return Qt;
}

DEFUN ("wnn-server-dict-delete", Fwnn_dict_delete, 1, 1, 0, /*
Remove dictionary specified by DIC-NUMBER from buffer.
*/
     (dicno))
{
  int	no;
  int	snum;
  CHECK_INT (dicno);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  no = XINT (dicno);
  if (!wnnfns_buf[snum]) return Qnil;
  if (jl_dic_delete (wnnfns_buf[snum], no) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-dict-list", Fwnn_dict_list, 0, 0, 0, /*
Return information of dictionaries.
*/
     ())
{
  WNN_DIC_INFO	*dicinfo;
  int		cnt, i;
  Ibyte		comment[1024];
  Lisp_Object	val;
  int	snum;
  Lisp_Object charset;

  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  if (!wnnfns_buf[snum]) return Qnil;
#ifdef	WNN6
  if((cnt = jl_fi_dic_list (wnnfns_buf[snum], 0x3f, &dicinfo)) < 0)
    return Qnil;
#else
  if((cnt = jl_dic_list (wnnfns_buf[snum], &dicinfo)) < 0) return Qnil;
#endif
  val = Qnil;
  for (i = 0, dicinfo += cnt; i < cnt; i++)
    {
      dicinfo--;
      w2m (dicinfo->comment, comment, charset);
      val =
	Fcons (Fcons (make_int (dicinfo->dic_no),
		      list4 (build_extstring (dicinfo->fname, Qfile_name),
			     build_istring (comment),
			     make_int (dicinfo->gosuu),
			     make_int (dicinfo->nice))), val);
    }
  return val;
}

DEFUN ("wnn-server-dict-comment", Fwnn_dict_comment, 2, 2, 0, /*
Set comment to dictionary specified by DIC-NUMBER.
Comment string COMMENT.
*/
     (dicno, comment))
{
  w_char wbuf[512];
  int snum;
  CHECK_INT (dicno);
  CHECK_STRING (comment);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  m2w (XSTRING_DATA (comment), wbuf);
  if (jl_dic_comment_set (wnnfns_buf[snum], XINT (dicno), wbuf) < 0)
    return Qnil;
  return Qt;
}


DEFUN ("wnn-server-set-rev", Fwnn_set_rev, 1, 1, 0, /*
Switch the translation mode to normal if T, or reverse if NIL.
*/
     (rev))
{
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (NILP (rev))
    {
      if ((!wnnfns_buf[snum]) || (!wnnfns_env_norm[snum])) return Qnil;
      jl_env_set (wnnfns_buf[snum], wnnfns_env_norm[snum]);
      wnnfns_norm = 1;
    }
  else
    {
      if ((!wnnfns_buf[snum]) || (!wnnfns_env_rev[snum])) return Qnil;
      jl_env_set (wnnfns_buf[snum], wnnfns_env_rev[snum]);
      wnnfns_norm = 0;
    }
  return Qt;
}

DEFUN ("wnn-server-henkan-begin", Fwnn_begin_henkan, 1, 1, 0, /*
Translate YOMI string to kanji. Retuen the number of bunsetsu.
*/
     (hstring))
{
  int			cnt;
  w_char		wbuf[5000];
  int	snum;
  CHECK_STRING (hstring);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  m2w (XSTRING_DATA (hstring), wbuf);
  if (snum == WNNSERVER_C)
    w2y (wbuf);

#ifdef	WNN6
  if ((cnt = jl_fi_ren_conv (wnnfns_buf[snum], wbuf,	0, -1, WNN_USE_MAE)) < 0)
    return Qnil;
#else
  if ((cnt = jl_ren_conv (wnnfns_buf[snum], wbuf,	0, -1, WNN_USE_MAE)) < 0)
    return Qnil;
#endif
  return make_int (cnt);
}

DEFUN ("wnn-server-zenkouho", Fwnn_zenkouho, 2, 2, 0, /*
Get zenkouho at BUNSETSU-NUMBER. Second argument DAI is t.
if dai-bunsetsu, NIL if sho-bunsetsu. Return the current offset of zenkouho.
*/
     (bunNo, dai))
{
  int	no, offset;
  int	snum;
  int	uniq_level;
  CHECK_INT (bunNo);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  no = XINT (bunNo);
  if (EQ(Vwnn_uniq_level, Qwnn_no_uniq)) uniq_level = WNN_NO_UNIQ;
  else if (EQ(Vwnn_uniq_level, Qwnn_uniq)) uniq_level = WNN_UNIQ;
  else uniq_level = WNN_UNIQ_KNJ;
  if (NILP (dai))
    {
      if ((offset = jl_zenkouho (wnnfns_buf[snum],no,WNN_USE_MAE,
				 uniq_level)) < 0)
	return Qnil;
    }
  else
    {
      if ((offset = jl_zenkouho_dai (wnnfns_buf[snum], no, dai_end (no, snum),
				     WNN_USE_MAE, uniq_level)) < 0)
	return Qnil;
    }
  return make_int (offset);
}


DEFUN ("wnn-server-get-zenkouho", Fwnn_get_zenkouho, 1, 1, 0, /*
Get kanji string of KOUHO-NUMBER.
*/
     (kouhoNo))
{
  Ibyte		kanji_buf[256];
  w_char	wbuf[256];
  int	snum;
  Lisp_Object charset;
  CHECK_INT (kouhoNo);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  if (!wnnfns_buf[snum]) return Qnil;
  jl_get_zenkouho_kanji (wnnfns_buf[snum], XINT (kouhoNo), wbuf);
  w2m (wbuf, kanji_buf, charset);
  return build_istring (kanji_buf);
}

DEFUN ("wnn-server-zenkouho-bun", Fwnn_zenkouho_bun, 0, 0, 0, /*
For Wnn.
*/
     ())
{
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  return make_int (jl_zenkouho_bun (wnnfns_buf[snum]));
}

DEFUN ("wnn-server-zenkouho-suu", Fwnn_zenkouho_suu, 0, 0, 0, /*
Return the number of zen kouho.
*/
     ())
{
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  return make_int (jl_zenkouho_suu (wnnfns_buf[snum]));
}

DEFUN ("wnn-server-dai-top", Fwnn_dai_top, 1, 1, 0, /*
Return t if bunsetsu BUN-NUMBER is dai-bunsetsu.
*/
     (bunNo))
{
  int	snum;
  CHECK_INT (bunNo);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (jl_dai_top (wnnfns_buf[snum], XINT (bunNo)) == 1) return Qt;
  else return Qnil;
}

DEFUN ("wnn-server-dai-end", Fwnn_dai_end, 1, 1, 0, /*
Return the bunsetu number of the next dai-bunsetsu after BUN-NUMBER.
*/
     (bunNo))
{
  int	snum;
  CHECK_INT (bunNo);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  return make_int (dai_end (XINT (bunNo), snum));
}

DEFUN ("wnn-server-henkan-kakutei", Fwnn_kakutei, 2, 2, 0, /*
Set candidate with OFFSET, DAI. DAI is T if dai-bunsetsu.
*/
     (offset, dai))
{
  int	snum;
  CHECK_INT (offset);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (NILP (dai))
    {
      if (jl_set_jikouho (wnnfns_buf[snum], XINT (offset)) < 0) return Qnil;
    }
  else
    {
      if (jl_set_jikouho_dai (wnnfns_buf[snum], XINT (offset)) < 0)
	return Qnil;
    }
  return Qt;
}

DEFUN ("wnn-server-bunsetu-henkou", Fwnn_bunsetu_henkou, 3, 3, 0, /*
Change length of BUN-NUMBER bunsetu to LEN. DAI is T if dai-bunsetsu.
*/
     (bunNo, len, dai))
{
  int		cnt, no;
  int	snum;
  CHECK_INT (bunNo);
  CHECK_INT (len);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  no = XINT (bunNo);
#ifdef	WNN6
  if ((cnt = jl_fi_nobi_conv (wnnfns_buf[snum], no, XINT(len), -1, WNN_USE_MAE,
			      NILP (dai) ? WNN_SHO : WNN_DAI)) < 0)
    return Qnil;
#else
  if ((cnt = jl_nobi_conv (wnnfns_buf[snum], no, XINT(len), -1, WNN_USE_MAE,
			   NILP (dai) ? WNN_SHO : WNN_DAI)) < 0)
    return Qnil;
#endif
  return make_int (cnt);
}

DEFUN ("wnn-server-inspect", Fwnn_inspect, 1, 1, 0, /*
Get bunsetsu information specified by BUN-NUMBER.
*/
     (bunNo))
{
  Lisp_Object		val;
  Ibyte			cbuf[512];
  w_char		wbuf[256];
  int			bun_no, yomilen, jirilen, i;
  int	snum;
  Lisp_Object		charset;
  CHECK_INT (bunNo);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  if (!wnnfns_buf[snum]) return Qnil;
  bun_no = XINT (bunNo);
  val = Qnil;
  val = Fcons (make_int (wnnfns_buf[snum]->bun[bun_no]->kangovect), val);
  val = Fcons (make_int (wnnfns_buf[snum]->bun[bun_no]->daihyoka), val);
  val = Fcons (make_int (wnnfns_buf[snum]->bun[bun_no]->hyoka), val);
  val = Fcons (make_int (wnnfns_buf[snum]->bun[bun_no]->ima), val);
  val = Fcons (make_int (wnnfns_buf[snum]->bun[bun_no]->hindo), val);
  val = Fcons (make_int (wnnfns_buf[snum]->bun[bun_no]->hinsi), val);
  val = Fcons (make_int (wnnfns_buf[snum]->bun[bun_no]->entry), val);
  val = Fcons (make_int (wnnfns_buf[snum]->bun[bun_no]->dic_no), val);
  yomilen = jl_get_yomi (wnnfns_buf[snum], bun_no, bun_no + 1, wbuf);
  jirilen = wnnfns_buf[snum]->bun[bun_no]->jirilen;
  for (i = yomilen; i >= jirilen; i--) wbuf[i+1] = wbuf[i];
  wbuf[jirilen] = '+';
  w2m (wbuf, cbuf, charset);
  val = Fcons (build_istring (cbuf), val);
  jl_get_kanji (wnnfns_buf[snum], bun_no, bun_no + 1, wbuf);
  w2m (wbuf, cbuf, charset);
  return Fcons (build_istring (cbuf), val);
}


DEFUN ("wnn-server-henkan-quit", Fwnn_quit_henkan, 0, 0, 0, /*
do nothing.
*/
     ())
{
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-bunsetu-kanji", Fwnn_bunsetu_kanji, 1, 1, 0, /*
Get the pair of kanji and length of bunsetsu specified by BUN-NUMBER.
*/
     (bunNo))
{
  int		no;
  Ibyte			kanji_buf[256];
  w_char		wbuf[256];
  int			kanji_len;
  int			snum;
  Lisp_Object		charset;
  CHECK_INT (bunNo);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  if (!wnnfns_buf[snum]) return Qnil;
  no = XINT (bunNo);
  kanji_len = jl_get_kanji (wnnfns_buf[snum], no, no + 1, wbuf);
  w2m (wbuf, kanji_buf, charset);
  return Fcons (build_istring (kanji_buf), make_int (kanji_len));
}

DEFUN ("wnn-server-bunsetu-yomi", Fwnn_bunsetu_yomi, 1, 1, 0, /*
Get the pair of yomi and length of bunsetsu specified by BUN-NUMBER.
*/
     (bunNo))
{
  int		no;
  Ibyte			yomi_buf[256];
  w_char		wbuf[256];
  int			yomi_len;
  int			snum;
  Lisp_Object		charset;
  CHECK_INT (bunNo);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  if (!wnnfns_buf[snum]) return Qnil;
  no = XINT (bunNo);
  yomi_len = jl_get_yomi (wnnfns_buf[snum], no, no + 1, wbuf);
  w2m (wbuf, yomi_buf, charset);
  return Fcons (build_istring (yomi_buf), make_int (yomi_len));
}

DEFUN ("wnn-server-bunsetu-suu", Fwnn_bunsetu_suu, 0, 0, 0, /*
Get the number of bunsetsu.
*/
     ())
{
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  return make_int (jl_bun_suu (wnnfns_buf[snum]));
}

DEFUN ("wnn-server-hindo-update", Fwnn_hindo_update, 0, 1, 0, /*
Update frequency of bunsetsu specified by NUM-NUMBER.
*/
     (bunNo))
{
  int		no;
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (NILP (bunNo)) no = -1;
  else
    {
      CHECK_INT (bunNo);
      no = XINT (bunNo);
    }
  if (!wnnfns_buf[snum]) return Qnil;
#ifdef	WNN6
  if (jl_optimize_fi (wnnfns_buf[snum], 0, no) < 0) return Qnil;
#else
  if (jl_update_hindo (wnnfns_buf[snum], 0, no) < 0) return Qnil;
#endif
  return Qt;
}


DEFUN ("wnn-server-word-add", Fwnn_word_toroku, 5, 5, 0, /*
Add a word to dictionary. Arguments are
DIC-NUMBER, KANJI, YOMI, COMMENT, HINSI-NUMBER.
*/
     (dicno, kanji, yomi, comment, hinsi))
{
  w_char		yomi_buf[256], kanji_buf[256], comment_buf[256];
  int	snum;
  CHECK_INT (dicno);
  CHECK_STRING (kanji);
  CHECK_STRING (yomi);
  CHECK_STRING (comment);
  CHECK_INT (hinsi);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  m2w (XSTRING_DATA (yomi), yomi_buf);
  if (snum == WNNSERVER_C)
    w2y (yomi_buf);
  m2w (XSTRING_DATA (kanji), kanji_buf);
  m2w (XSTRING_DATA (comment), comment_buf);
  if (jl_word_add (wnnfns_buf[snum], XINT (dicno), yomi_buf, kanji_buf,
		   comment_buf, XINT (hinsi), 0) < 0)
    return Qnil;
  else return Qt;
}


DEFUN ("wnn-server-word-delete", Fwnn_word_sakujo, 2, 2, 0, /*
Delete a word from dictionary, specified by DIC-NUMBER, SERIAL-NUMBER.
*/
     (no, serial))
{
  int	snum;
  CHECK_INT (no);
  CHECK_INT (serial);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (jl_word_delete (wnnfns_buf[snum], XINT (no), XINT (serial)) < 0)
    return Qnil;
  else return Qt;
}


DEFUN ("wnn-server-word-use", Fwnn_word_use, 2, 2, 0, /*
Toggle on/off word, specified by DIC-NUMBER and SERIAL-NUMBER.
*/
     (no, serial))
{
  int	snum;
  CHECK_INT (no);
  CHECK_INT (serial);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (jl_word_use (wnnfns_buf[snum], XINT (no), XINT (serial)) < 0)
    return Qnil;
  else return Qt;
}

DEFUN ("wnn-server-word-info", Fwnn_word_info, 2, 2, 0, /*
Return list of yomi, kanji, comment, hindo, hinshi.
*/
     (no, serial))
{
  Lisp_Object		val;
  struct wnn_jdata	*info_buf;
  Ibyte			cbuf[512];
  int			snum;
  Lisp_Object		charset;
  CHECK_INT (no);
  CHECK_INT (serial);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  if (!wnnfns_buf[snum]) return Qnil;
  if ((info_buf =  jl_word_info (wnnfns_buf[snum],
				 XINT (no), XINT (serial))) != NULL)
    {
      return Qnil;
    }
  else
    {
      val = Qnil;
      val = Fcons (make_int (info_buf->hinshi), val);
      val = Fcons (make_int (info_buf->hindo), val);
      w2m (info_buf->com, cbuf, charset);
      val = Fcons (build_istring (cbuf), val);
      w2m (info_buf->kanji, cbuf, charset);
      val = Fcons (build_istring (cbuf), val);
      w2m (info_buf->yomi, cbuf, charset);
      val = Fcons (build_istring (cbuf), val);
      return val;
    }
}

DEFUN ("wnn-server-word-hindo-set", Fwnn_hindo_set, 3, 3, 0, /*
Set frequency to arbitrary value. Specified by DIC-NUMBER,
SERIAL-NUMBER, FREQUENCY.
*/
     (no, serial, hindo))
{
  int	snum;
  CHECK_INT (no);
  CHECK_INT (serial);
  CHECK_INT (hindo);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (js_hindo_set (jl_env_get (wnnfns_buf[snum]),
		    XINT (no),
		    XINT (serial),
		    WNN_HINDO_NOP,
		    XINT (hindo)) < 0)
    return Qnil;
  else return Qt;
}


DEFUN ("wnn-server-word-search", Fwnn_dict_search, 1, 1, 0, /*
Search a word YOMI from buffer.
Return list of (kanji hinshi freq dic_no serial).
*/
     (yomi))
{
  Lisp_Object		val;
  struct wnn_jdata	*wordinfo;
  int			i, count;
  w_char		wbuf[256];
  Ibyte			kanji_buf[256];
  int			snum;
  Lisp_Object		charset;
  CHECK_STRING (yomi);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  if (!wnnfns_buf[snum]) return Qnil;
  m2w (XSTRING_DATA (yomi), wbuf);
  if (snum == WNNSERVER_C)
    w2y (wbuf);
  if ((count = jl_word_search_by_env (wnnfns_buf[snum],
				      wbuf, &wordinfo)) < 0)
    return Qnil;
  val = Qnil;
  for (i = 0, wordinfo += count; i < count; i++)
    {
      wordinfo--;
      w2m (wordinfo->kanji, kanji_buf, charset);
      val = Fcons (Fcons (build_istring (kanji_buf),
			  list4 (make_int (wordinfo->hinshi),
				 make_int (wordinfo->hindo),
				 make_int (wordinfo->dic_no),
				 make_int (wordinfo->serial))),
		   val);
    }
  return val;
}

DEFUN ("wnn-server-dict-save", Fwnn_dict_save, 0, 0, 0, /*
Save all dictionaries and frequency files.
*/
     ())
{
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (jl_dic_save_all (wnnfns_buf[snum]) < 0) return Qnil;
  else return Qt;
}

DEFUN ("wnn-server-get-param", Fwnn_get_param, 0, 0, 0, /*
Returns (n nsho hindo len jiri flag jisho sbn dbn_len sbn_cnt
suuji kana eisuu kigou toji_kakko fuzokogo kaikakko)
*/
     ())
{
  struct wnn_param	param;
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (jl_param_get (wnnfns_buf[snum], &param) < 0) return Qnil;
  return Fcons (make_int (param.n),
	 Fcons (make_int (param.nsho),
	 Fcons (make_int (param.p1),
	 Fcons (make_int (param.p2),
	 Fcons (make_int (param.p3),
	 Fcons (make_int (param.p4),
	 Fcons (make_int (param.p5),
	 Fcons (make_int (param.p6),
	 Fcons (make_int (param.p7),
	 Fcons (make_int (param.p8),
	 Fcons (make_int (param.p9),
	 Fcons (make_int (param.p10),
	 Fcons (make_int (param.p11),
	 Fcons (make_int (param.p12),
	 Fcons (make_int (param.p13),
	 Fcons (make_int (param.p14),
	 Fcons (make_int (param.p15),Qnil)))))))))))))))));
}

DEFUN ("wnn-server-set-param", Fwnn_set_param, 1, 1, 0, /*
Set parameters using an alist, where the CAR contains one of
wnn_n, wnn_nsho, wnn_hindo, wnn_len, wnn_jiri, wnn_flag,
wnn_jisho, wnn_sbn, wnn_dbn_len, wnn_sbn_cnt, wnn_suuji,
wnn_kana, wnn_eisuu, wnn_kigou, wnn_toji_kakko, wnn_fuzokogo,
or wnn_kaikakko and the CDR contains the value.
*/
     (Vsetvalues_alist))
{
  int             rc;
  struct wnn_param	param;
  int	snum;

  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  rc = jl_param_get (wnnfns_buf[snum], &param);
  if (rc < 0) return Qnil;

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, val, Vsetvalues_alist)
      {
	int setval;
	CHECK_INT (val);
	setval = XINT (val);
	if (EQ (key, Qwnn_n)) param.n = setval;
	else if (EQ (key, Qwnn_nsho)) param.nsho = setval;
	else if (EQ (key, Qwnn_hindo)) param.p1 = setval;
	else if (EQ (key, Qwnn_len)) param.p2 = setval;
	else if (EQ (key, Qwnn_jiri)) param.p3 = setval;
	else if (EQ (key, Qwnn_flag)) param.p4 = setval;
	else if (EQ (key, Qwnn_jisho)) param.p5 = setval;
	else if (EQ (key, Qwnn_sbn)) param.p6 = setval;
	else if (EQ (key, Qwnn_dbn_len)) param.p7 = setval;
	else if (EQ (key, Qwnn_sbn_cnt)) param.p8 = setval;
	else if (EQ (key, Qwnn_suuji)) param.p9 = setval;
	else if (EQ (key, Qwnn_kana)) param.p10 = setval;
	else if (EQ (key, Qwnn_eisuu)) param.p11 = setval;
	else if (EQ (key, Qwnn_kigou)) param.p12 = setval;
	else if (EQ (key, Qwnn_toji_kakko)) param.p13 = setval;
	else if (EQ (key, Qwnn_fuzokogo)) param.p14 = setval;
	else if (EQ (key, Qwnn_kaikakko)) param.p15 = setval;
	else
	  {
	    invalid_constant ("Invalid wnn keyword", key);
	    return Qnil;
	  }
      }
  }

#if 0
  printf("wnn_n = %d\n",param.n);
  printf("wnn_nsho = %d\n",param.nsho);
  printf("wnn_hindo = %d\n",param.p1);
  printf("wnn_len = %d\n",param.p2);
  printf("wnn_jiri = %d\n",param.p3);
  printf("wnn_flag = %d\n",param.p4);
  printf("wnn_jisho = %d\n",param.p5);
  printf("wnn_sbn = %d\n",param.p6);
  printf("wnn_dbn_len = %d\n",param.p7);
  printf("wnn_sbn_cnt = %d\n",param.p8);
  printf("wnn_suuji = %d\n",param.p9);
  printf("wnn_kana = %d\n",param.p10);
  printf("wnn_eisuu = %d\n",param.p11);
  printf("wnn_kigou = %d\n",param.p12);
  printf("wnn_toji_kakko = %d\n",param.p13);
  printf("wnn_fuzokogo = %d\n",param.p14);
  printf("wnn_kaikakko = %d\n",param.p15);
#endif

  rc = jl_param_set (wnnfns_buf[snum], &param);
  if (rc < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-get-msg", Fwnn_get_msg, 0, 0, 0, /*
Get message string from wnn_perror.
*/
     ())
{
  Ibyte		mbuf[256];
  char 		*msgp;
  int		snum;
  Lisp_Object	charset;
  char		langname[32];
/*  CHECK_INT (errno);*/
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  switch (snum)
    {
    case WNNSERVER_J:
      strcpy (langname, "ja_JP");
      break;
    case WNNSERVER_C:
      strcpy (langname, "zh_CN");
      break;
/*
  case WNNSERVER_T:
  strcpy (langname, "zh_TW");
  break;
  */
    case WNNSERVER_K:
      strcpy (langname, "ko_KR");
      break;
    }
  if (!wnnfns_buf[snum]) return Qnil;
/*  msgp = msg_get (wnn_msg_cat, XINT (errno), 0, 0);*/
  msgp = wnn_perror_lang (langname);
  c2m ((UExtbyte *) msgp, mbuf, charset);
  return build_istring (mbuf);
}


DEFUN ("wnn-server-fuzokugo-set", Fwnn_fuzokugo_set, 1, 1, 0, /*
For Wnn.
*/
     (file))
{
  int	snum;
  CHECK_STRING (file);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (jl_fuzokugo_set (wnnfns_buf[snum],
		       LISP_STRING_TO_EXTERNAL (file, Qfile_name)) < 0)
    return Qnil;
  return Qt;
}

DEFUN ("wnn-server-fuzokugo-get", Fwnn_fuzokugo_get, 0, 0, 0, /*
For Wnn.
*/
     ())
{
  char	fname[256];
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (jl_fuzokugo_get (wnnfns_buf[snum], fname) < 0) return Qnil;
  return build_extstring (fname, Qfile_name);
}


DEFUN ("wnn-server-isconnect", Fwnn_isconnect, 0, 0, 0, /*
For Wnn.
*/
     ())
{
  int	snum;
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if (jl_isconnect (wnnfns_buf[snum])) return Qt;
  else return Qnil;
}

DEFUN ("wnn-server-hinsi-dicts", Fwnn_hinsi_dicts, 1, 1, 0, /*
For Wnn.
*/
     (hinsi))
{
  int		*area;
  int		cnt;
  Lisp_Object	val;
  int	snum;
  CHECK_INT (hinsi);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  if ((cnt = jl_hinsi_dicts (wnnfns_buf[snum], XINT (hinsi), &area)) < 0)
    return Qnil;
  val = Qnil;
  for (area += cnt; cnt > 0; cnt--)
    {
      area--;
      val = Fcons (make_int (*area), val);
    }
  return val;
}

DEFUN ("wnn-server-hinsi-list", Fwnn_hinsi_list, 2, 2, 0, /*
For Wnn.
*/
     (dicno, name))
{
  int		cnt;
  Lisp_Object	val;
  w_char	wbuf[256];
  w_char	**area;
  Ibyte		cbuf[512];
  int		snum;
  Lisp_Object	charset;
  CHECK_INT (dicno);
  CHECK_STRING (name);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  if (!wnnfns_buf[snum]) return Qnil;
  m2w (XSTRING_DATA (name), wbuf);
  if ((cnt = jl_hinsi_list (wnnfns_buf[snum], XINT (dicno), wbuf, &area)) < 0)
    return Qnil;
  if (cnt == 0) return make_int (0);
  val = Qnil;
  for (area += cnt; cnt > 0; cnt--)
    {
      area--;
      w2m (*area, cbuf, charset);
      val = Fcons (build_istring (cbuf), val);
    }
  return val;
}

DEFUN ("wnn-server-hinsi-name", Fwnn_hinsi_name, 1, 1, 0, /*
For Wnn.
*/
     (no))
{
  Ibyte		name[256];
  w_char	*wname;
  int		snum;
  Lisp_Object	charset;
  CHECK_INT (no);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  charset = charset_wnn_server_type[snum];
  if (!wnnfns_buf[snum]) return Qnil;
  if ((wname = jl_hinsi_name (wnnfns_buf[snum], XINT (no))) == 0) return Qnil;
  w2m (wname, name, charset);
  return build_istring (name);
}
#ifdef	WNN6
DEFUN ("wnn-server-fisys-dict-add", Fwnn_fisys_dict_add, 3, MANY, 0, /*
Add dictionary specified by FISYS-DICT-FILE-NAME, FISYS-FREQ-FILE-NAME,
FISYS-FREQ-FILE-MODE.
Specify password files of dictionary and frequency, PW1 and PW2, if needed.
*/
     (int nargs, Lisp_Object *args))
{
  struct gcpro gcpro1;
  int   snum;
  CHECK_STRING (args[0]);
  CHECK_STRING (args[1]);
  if (!NILP (args[3])) CHECK_STRING (args[3]);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  GCPRO1 (*args);
  gcpro1.nvars = nargs;
  if (jl_fi_dic_add (wnnfns_buf[snum],
		     LISP_STRING_TO_EXTERNAL (args[0], Qfile_name),
		     LISP_STRING_TO_EXTERNAL (args[1], Qfile_name),
		     WNN_FI_SYSTEM_DICT,
		     WNN_DIC_RDONLY,
		     NILP (args[2]) ? WNN_DIC_RDONLY : WNN_DIC_RW,
		     0,
		     NILP (args[3]) ? 0 :
		     LISP_STRING_TO_EXTERNAL (args[3], Qfile_name),
		     yes_or_no,
		     puts2) < 0)
    {
      UNGCPRO;
      return Qnil;
    }
  UNGCPRO;
  return Qt;
}

DEFUN ("wnn-server-fiusr-dict-add", Fwnn_fiusr_dict_add, 4, MANY, 0, /*
Add dictionary specified by FIUSR-DICT-FILE-NAME, FIUSR-FREQ-FILE-NAME,
FIUSR-DICT-FILE-MODE, FIUSR-FREQ-FILE-MODE.
Specify password files of dictionary and frequency, PW1 and PW2, if needed.
*/
     (int nargs, Lisp_Object *args))
{
  struct gcpro gcpro1;
  int   snum;
  CHECK_STRING (args[0]);
  CHECK_STRING (args[1]);
  if (!NILP (args[4])) CHECK_STRING (args[4]);
  if (!NILP (args[5])) CHECK_STRING (args[5]);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  GCPRO1 (*args);
  gcpro1.nvars = nargs;
  if (jl_fi_dic_add (wnnfns_buf[snum],
		     LISP_STRING_TO_EXTERNAL (args[0], Qfile_name),
		     LISP_STRING_TO_EXTERNAL (args[1], Qfile_name),
		     WNN_FI_USER_DICT,
		     NILP (args[2]) ? WNN_DIC_RDONLY : WNN_DIC_RW,
		     NILP (args[3]) ? WNN_DIC_RDONLY : WNN_DIC_RW,
		     NILP (args[4]) ? 0 :
		     LISP_STRING_TO_EXTERNAL (args[4], Qfile_name),
		     NILP (args[5]) ? 0 :
		     LISP_STRING_TO_EXTERNAL (args[5], Qfile_name),
		     yes_or_no,
		     puts2) < 0)
    {
      UNGCPRO;
      return Qnil;
    }
  UNGCPRO;
  return Qt;
}

DEFUN ("wnn-server-notrans-dict-add", Fwnn_notrans_dict_add, 3, MANY, 0, /*
Add dictionary specified by NOTRANS-DICT-FILE-NAME, PRIORITY, DICT-FILE-MODE.
Specify password files of dictionary and frequency PW1 if needed.
*/
     (int nargs, Lisp_Object *args))
{
  struct gcpro gcpro1;
  int   snum;
  int	dic_no;
  struct wnn_env *cur_env;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  CHECK_STRING (args[0]);
  CHECK_INT (args[1]);
  if (! NILP (args[3])) CHECK_STRING (args[3]);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  GCPRO1 (*args);
  gcpro1.nvars = nargs;
  if(wnnfns_norm)
      cur_env = wnnfns_env_norm[snum];
  else
      cur_env = wnnfns_env_rev[snum];
  dic_no = js_get_autolearning_dic(cur_env, WNN_MUHENKAN_LEARNING);
  if (dic_no == WNN_NO_LEARNING)
    {
      if ((dic_no = jl_dic_add (wnnfns_buf[snum],
				LISP_STRING_TO_EXTERNAL (args[0],
							     Qfile_name),
				0,
				wnnfns_norm ? WNN_DIC_ADD_NOR :
				WNN_DIC_ADD_REV,
				XINT (args[1]),
				WNN_DIC_RW, WNN_DIC_RW,
				NILP (args[3]) ? 0 :
				LISP_STRING_TO_EXTERNAL (args[3],
							     Qfile_name),
				0,
				yes_or_no,
				puts2)) < 0)
	{
	  UNGCPRO;
	  return Qnil;
	}
      js_set_autolearning_dic (cur_env, WNN_MUHENKAN_LEARNING, dic_no);
    }
  if (!js_is_loaded_temporary_dic (cur_env))
    {
      if (js_temporary_dic_add (cur_env,
				wnnfns_norm ? WNN_DIC_ADD_NOR :
				WNN_DIC_ADD_REV) < 0)
	{
	  UNGCPRO;
          return Qnil;
	}
    }
  vmask |= WNN_ENV_MUHENKAN_LEARN_MASK;
  henv.muhenkan_flag = NILP (args[2]) ? WNN_DIC_RDONLY : WNN_DIC_RW;
  if (jl_set_henkan_env (wnnfns_buf[snum],
			 vmask,
			 &henv) < 0)
    {
      UNGCPRO;
      return Qnil;
    }
  UNGCPRO;
  return Qt;
}

DEFUN ("wnn-server-bmodify-dict-add", Fwnn_bmodify_dict_add, 3, MANY, 0, /*
Add dictionary specified by BMODIFY-DICT-FILE-NAME, PRIORITY, DICT-FILE-MODE.
Specify password files of dictionary and frequency PW1 if needed.
*/
     (int nargs, Lisp_Object *args))
{
  struct gcpro gcpro1;
  int   snum;
  int   dic_no;
  struct wnn_env *cur_env;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  CHECK_STRING (args[0]);
  CHECK_INT (args[1]);
  if (! NILP (args[3])) CHECK_STRING (args[3]);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  GCPRO1 (*args);
  gcpro1.nvars = nargs;
  if(wnnfns_norm)
      cur_env = wnnfns_env_norm[snum];
  else
      cur_env = wnnfns_env_rev[snum];
  dic_no = js_get_autolearning_dic(cur_env, WNN_BUNSETSUGIRI_LEARNING);
  if (dic_no == WNN_NO_LEARNING)
    {
      if ((dic_no = jl_dic_add (wnnfns_buf[snum],
				LISP_STRING_TO_EXTERNAL (args[0],
							 Qfile_name),
				0,
				wnnfns_norm ? WNN_DIC_ADD_NOR :
				WNN_DIC_ADD_REV,
				XINT(args[1]),
				WNN_DIC_RW, WNN_DIC_RW,
				NILP (args[3]) ? 0 :
				LISP_STRING_TO_EXTERNAL (args[3],
							 Qfile_name),
				0,
				yes_or_no,
				puts2)) < 0)
	{
          UNGCPRO;
          return Qnil;
	}
      js_set_autolearning_dic (cur_env, WNN_BUNSETSUGIRI_LEARNING, dic_no);
    }
  if (!js_is_loaded_temporary_dic (cur_env))
    {
      if (js_temporary_dic_add (cur_env,
				wnnfns_norm ? WNN_DIC_ADD_NOR :
				WNN_DIC_ADD_REV) < 0)
	{
          UNGCPRO;
          return Qnil;
	}
    }
  vmask |= WNN_ENV_BUNSETSUGIRI_LEARN_MASK;
  henv.bunsetsugiri_flag = NILP (args[2]) ? WNN_DIC_RDONLY : WNN_DIC_RW;
  if (jl_set_henkan_env (wnnfns_buf[snum],
			 vmask,
			 &henv) < 0)
    {
      UNGCPRO;
      return Qnil;
    }
  UNGCPRO;
  return Qt;
}

DEFUN ("wnn-server-set-last-is-first", Fwnn_last_is_first, 1, 1, 0, /*
For FI-Wnn.
*/
     (mode))
{
  int   snum;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  vmask |= WNN_ENV_LAST_IS_FIRST_MASK;
  henv.last_is_first_flag = NILP (mode) ? False : True;
  if(jl_set_henkan_env(wnnfns_buf[snum],
		       vmask,
		       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-complex-conv-mode", Fwnn_complex_conv, 1, 1, 0, /*
For FI-Wnn.
*/
     (mode))
{
  int   snum;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  vmask |= WNN_ENV_COMPLEX_CONV_MASK;
  henv.complex_flag = NILP (mode) ? False : True;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-okuri-learn-mode", Fwnn_okuri_learn, 1, 1, 0, /*
For FI-Wnn.
*/
     (mode))
{
  int   snum;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  vmask |= WNN_ENV_OKURI_LEARN_MASK;
  henv.okuri_learn_flag = NILP (mode) ? False : True;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-okuri-flag", Fwnn_okuri_flag, 1, 1, 0, /*
For FI-Wnn.
*/
     (lmode))
{
  int   snum, mode;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  CHECK_INT (lmode);
  mode = XINT (lmode);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  if(mode != WNN_OKURI_REGULATION &&
     mode != WNN_OKURI_NO &&
     mode != WNN_OKURI_YES)
      return Qnil;
  else
      henv.okuri_flag = mode;
  vmask |= WNN_ENV_OKURI_MASK;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-prefix-learn-mode", Fwnn_prefix_learn, 1, 1, 0, /*
For FI-Wnn.
*/
     (mode))
{
  int   snum;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  vmask |= WNN_ENV_PREFIX_LEARN_MASK;
  henv.prefix_learn_flag = NILP (mode) ? False : True;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-prefix-flag", Fwnn_prefix_flag, 1, 1, 0, /*
For FI-Wnn.
*/
     (lmode))
{
  int   snum, mode;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  CHECK_INT (lmode);
  mode = XINT (lmode);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  if(mode != WNN_KANA_KOUHO && mode != WNN_KANJI_KOUHO)
      return Qnil;
  else
      henv.prefix_flag = mode;
  vmask |= WNN_ENV_PREFIX_MASK;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-suffix-learn-mode", Fwnn_suffix_learn, 1, 1, 0, /*
For FI-Wnn.
*/
     (mode))
{
  int   snum;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  vmask |= WNN_ENV_SUFFIX_LEARN_MASK;
  henv.suffix_learn_flag = NILP (mode) ? False : True;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-common-learn-mode", Fwnn_common_learn, 1, 1, 0, /*
For FI-Wnn.
*/
     (mode))
{
  int   snum;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  vmask |= WNN_ENV_COMMON_LAERN_MASK;
  henv.common_learn_flag = NILP (mode) ? False : True;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-freq-func-mode", Fwnn_freq_func, 1, 1, 0, /*
For FI-Wnn.
*/
     (lmode))
{
  int   snum, mode;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  CHECK_INT (lmode);
  mode = XINT (lmode);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  if(mode != 0 && mode != 1 && mode != 2 && mode != 3 && mode != 4)
      return Qnil;
  else
      henv.freq_func_flag = mode;
  vmask |= WNN_ENV_FREQ_FUNC_MASK;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-numeric-mode", Fwnn_numeric, 1, 1, 0, /*
For FI-Wnn.
*/
     (lmode))
{
  int   snum, mode;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  CHECK_INT (lmode);
  mode = XINT (lmode);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  if(mode != WNN_NUM_KANSUUJI &&
     mode != WNN_NUM_KANOLD &&
     mode != WNN_NUM_HANCAN &&
     mode != WNN_NUM_ZENCAN &&
     mode != WNN_NUM_HAN &&
     mode != WNN_NUM_ZEN &&
     mode != WNN_NUM_KAN)
      return Qnil;
  else
      henv.numeric_flag = mode;
  vmask |= WNN_ENV_NUMERIC_MASK;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-alphabet-mode", Fwnn_alphabet, 1, 1, 0, /*
For FI-Wnn.
*/
     (lmode))
{
  int   snum, mode;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  CHECK_INT (lmode);
  mode = XINT (lmode);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  if(mode != WNN_ALP_HAN && mode != WNN_ALP_ZEN)
      return Qnil;
  else
      henv.alphabet_flag = mode;
  vmask |= WNN_ENV_ALPHABET_MASK;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-symbol-mode", Fwnn_symbol, 1, 1, 0, /*
For FI-Wnn.
*/
     (lmode))
{
  int   snum, mode;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  CHECK_INT (lmode);
  mode = XINT (lmode);
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  if(mode != WNN_KIG_HAN && mode != WNN_KIG_JIS && mode != WNN_KIG_ASC)
      return Qnil;
  else
      henv.symbol_flag = mode;
  vmask |= WNN_ENV_SYMBOL_MASK;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-server-set-yuragi-mode", Fwnn_yuragi, 1, 1, 0, /*
For FI-Wnn.
*/
     (mode))
{
  int   snum;
  unsigned long vmask = 0;
  struct wnn_henkan_env henv;
  if ((snum = check_wnn_server_type()) == -1) return Qnil;
  if(!wnnfns_buf[snum]) return Qnil;
  vmask |= WNN_ENV_YURAGI_MASK;
  henv.yuragi_flag = NILP (mode) ? False : True;
  if(jl_set_henkan_env(wnnfns_buf[snum],
                       vmask,
                       &henv) < 0) return Qnil;
  return Qt;
}

DEFUN ("wnn-reset-previous-info", Fwnn_reset_prev, 0, 0, 0, /*
For FI-Wnn.
*/
    ())
{
    int   snum;
    if ((snum = check_wnn_server_type()) == -1) return Qnil;
    if(!wnnfns_buf[snum]) return Qnil;
    if(jl_reset_prev_bun(wnnfns_buf[snum]) < 0) return Qnil;
    return Qt;
}
#endif	/* Wnn6 */

DEFUN ("wnn-server-version", Fwnn_version, 0, 0, 0, /*
Returns Wnn server version ID.
*/
    ())
{
    int   snum;
    int   serv;
    int   libv;
    struct wnn_env *cur_env;
    if ((snum = check_wnn_server_type()) == -1) return Qnil;
    if(!wnnfns_buf[snum]) return Qnil;
    if(wnnfns_norm)
      cur_env = wnnfns_env_norm[snum];
    else
      cur_env = wnnfns_env_rev[snum];
    if(js_version (cur_env->js_id,&serv,&libv) < 0) return Qnil;
    return make_int (serv);
}

DEFUN ("wnn-server-hinsi-number", Fwnn_hinsi_number, 1, 1, 0, /*
For Wnn.
*/
     (name))
{
  w_char		w_buf[256];
  int		no;
  int	snum;
  CHECK_STRING (name);
  if ((snum = check_wnn_server_type ()) == -1) return Qnil;
  if (!wnnfns_buf[snum]) return Qnil;
  m2w (XSTRING_DATA (name), w_buf);
  if ((no = jl_hinsi_number (wnnfns_buf[snum], w_buf)) < 0) return Qnil;
  return make_int (no);
}

void
syms_of_mule_wnn (void)
{
  DEFSUBR (Fwnn_open);
  DEFSUBR (Fwnn_close);
  DEFSUBR (Fwnn_dict_add);
  DEFSUBR (Fwnn_dict_delete);
  DEFSUBR (Fwnn_dict_list);
  DEFSUBR (Fwnn_dict_comment);
  DEFSUBR (Fwnn_set_rev);
  DEFSUBR (Fwnn_begin_henkan);
  DEFSUBR (Fwnn_zenkouho);
  DEFSUBR (Fwnn_get_zenkouho);
  DEFSUBR (Fwnn_zenkouho_bun);
  DEFSUBR (Fwnn_zenkouho_suu);
  DEFSUBR (Fwnn_dai_top);
  DEFSUBR (Fwnn_dai_end);
  DEFSUBR (Fwnn_kakutei);
  DEFSUBR (Fwnn_bunsetu_henkou);
  DEFSUBR (Fwnn_inspect);
  DEFSUBR (Fwnn_quit_henkan);
  DEFSUBR (Fwnn_bunsetu_kanji);
  DEFSUBR (Fwnn_bunsetu_yomi);
  DEFSUBR (Fwnn_bunsetu_suu);
  DEFSUBR (Fwnn_hindo_update);
  DEFSUBR (Fwnn_word_toroku);
  DEFSUBR (Fwnn_word_sakujo);
  DEFSUBR (Fwnn_word_use);
  DEFSUBR (Fwnn_word_info);
  DEFSUBR (Fwnn_hindo_set);
  DEFSUBR (Fwnn_dict_search);
  DEFSUBR (Fwnn_dict_save);
  DEFSUBR (Fwnn_get_param);
  DEFSUBR (Fwnn_set_param);
  DEFSUBR (Fwnn_get_msg);
  DEFSUBR (Fwnn_fuzokugo_set);
  DEFSUBR (Fwnn_fuzokugo_get);
  DEFSUBR (Fwnn_isconnect);
  DEFSUBR (Fwnn_hinsi_dicts);
  DEFSUBR (Fwnn_hinsi_list);
  DEFSUBR (Fwnn_hinsi_name);
  DEFSUBR (Fwnn_hinsi_number);
#ifdef	WNN6
  DEFSUBR (Fwnn_fisys_dict_add);
  DEFSUBR (Fwnn_fiusr_dict_add);
  DEFSUBR (Fwnn_notrans_dict_add);
  DEFSUBR (Fwnn_bmodify_dict_add);
  DEFSUBR (Fwnn_last_is_first);
  DEFSUBR (Fwnn_complex_conv);
  DEFSUBR (Fwnn_okuri_learn);
  DEFSUBR (Fwnn_okuri_flag);
  DEFSUBR (Fwnn_prefix_learn);
  DEFSUBR (Fwnn_prefix_flag);
  DEFSUBR (Fwnn_suffix_learn);
  DEFSUBR (Fwnn_common_learn);
  DEFSUBR (Fwnn_freq_func);
  DEFSUBR (Fwnn_numeric);
  DEFSUBR (Fwnn_alphabet);
  DEFSUBR (Fwnn_symbol);
  DEFSUBR (Fwnn_yuragi);
  DEFSUBR (Fwnn_reset_prev);
#endif	/* Wnn6 */
  DEFSUBR (Fwnn_version);

  DEFSYMBOL (Qjserver);
  DEFSYMBOL (Qcserver);
  /* DEFSYMBOL (Qtserver); */
  DEFSYMBOL (Qkserver);

  DEFSYMBOL (Qwnn_no_uniq);
  DEFSYMBOL (Qwnn_uniq);
  DEFSYMBOL (Qwnn_uniq_kanji);
  defsymbol (&Qwnn_n, "wnn_n");
  defsymbol (&Qwnn_nsho, "wnn_nsho");
  defsymbol (&Qwnn_hindo, "wnn_hindo");
  defsymbol (&Qwnn_len, "wnn_len");
  defsymbol (&Qwnn_jiri, "wnn_jiri");
  defsymbol (&Qwnn_flag, "wnn_flag");
  defsymbol (&Qwnn_jisho, "wnn_jisho");
  defsymbol (&Qwnn_sbn, "wnn_sbn");
  defsymbol (&Qwnn_dbn_len, "wnn_dbn_len");
  defsymbol (&Qwnn_sbn_cnt, "wnn_sbn_cnt");
  defsymbol (&Qwnn_suuji, "wnn_suuji");
  defsymbol (&Qwnn_kana, "wnn_kana");
  defsymbol (&Qwnn_eisuu, "wnn_eisuu");
  defsymbol (&Qwnn_kigou, "wnn_kigou");
  defsymbol (&Qwnn_toji_kakko, "wnn_toji_kakko");
  defsymbol (&Qwnn_fuzokogo, "wnn_fuzokogo");
  defsymbol (&Qwnn_kaikakko, "wnn_kaikakko");
}

void
reinit_vars_of_mule_wnn (void)
{
  int i;

  for (i = 0; i < NSERVER; i++)
    {
      wnnfns_buf[i] = (struct wnn_buf *) 0;
      wnnfns_env_norm[i] = (struct wnn_env *) 0;
      wnnfns_env_rev[i] = (struct wnn_env *) 0;
    }

  charset_wnn_server_type[0] = Vcharset_japanese_jisx0208;
  charset_wnn_server_type[1] = Vcharset_chinese_gb2312;
  charset_wnn_server_type[2] = Vcharset_thai_tis620;
  charset_wnn_server_type[3] = Vcharset_korean_ksc5601;
}

void
vars_of_mule_wnn (void)
{
  DEFVAR_LISP ("wnn-server-type", &Vwnn_server_type /*
*jserver, cserver ..
*/ );
  DEFVAR_LISP ("cwnn-zhuyin", &Vcwnn_zhuyin /*
*pinyin or zhuyin
*/ );
  DEFVAR_LISP ("wnnenv-sticky", &Vwnnenv_sticky /*
*If non-nil, make environment sticky
*/ );
  DEFVAR_LISP ("wnn-uniq-level", &Vwnn_uniq_level /*
*Uniq level
*/ );

  Vwnn_server_type = Qjserver;
  Vcwnn_zhuyin = Qnil;
  Vwnnenv_sticky = Qnil;

  DEFSYMBOL (Qchinese_sisheng);

  Vwnn_uniq_level = Qwnn_uniq;

  Fprovide (intern ("wnn"));
}

/* Convert from the wide-char format expected for wnn to the XEmacs string
   format. */

void
w2m (w_char *wp, Ibyte *mp, Lisp_Object charset)
{
  w_char	wc;
  w_char	pzy[10];
  int		i, len;

  while ((wc = *wp++) != 0)
    {
      switch (wc & 0x8080)
	{
	case 0x80:
	  if (EQ(Vwnn_server_type, Qcserver))
	    {
	      len = cwnn_yincod_pzy (pzy, wc,
				     NILP (Vcwnn_zhuyin)
				     ? CWNN_PINYIN
				     : CWNN_ZHUYIN);
	      for (i = 0; i < len; i++)
		{
		  if (pzy[i] & 0x80)
		    mp += charset_codepoint_to_itext
		      (Fget_charset (Qchinese_sisheng), 0, pzy[i] & 0x7f, mp,
		       CONVERR_USE_PRIVATE);
		  else
		    /* @@#### Correct? */
		    mp += charset_codepoint_to_itext
		      (Vcharset_ascii, 0, pzy[i] & 0x7f, mp,
		       CONVERR_USE_PRIVATE);
		}
	    }
	  else
	    mp += charset_codepoint_to_itext (Vcharset_katakana_jisx0201,
					      0, wc & 0x7f, mp,
					      CONVERR_USE_PRIVATE);
	  break;
	case 0x8080:
	  mp += charset_codepoint_to_itext (charset, (wc & 0x7f00) >> 8,
					    wc & 0x007f, mp,
					    CONVERR_USE_PRIVATE);
	  break;
	case 0x8000:
	  {
	    Lisp_Object newchar = charset;
	    if (EQ (charset, Vcharset_japanese_jisx0208))
	      newchar = Vcharset_japanese_jisx0212;
#ifndef UNICODE_INTERNAL
	    /* @@#### Something very strange about this */
	    else if (EQ (charset, Vcharset_chinese_big5_1))
	      newchar = Vcharset_chinese_big5_2;
#endif /* not UNICODE_INTERNAL */
	    mp += charset_codepoint_to_itext (newchar, (wc & 0x7f00) >> 8,
					      wc & 0x007f, mp,
					      CONVERR_USE_PRIVATE);
	    break;
	  }
	default:
	  mp += set_itext_ichar (mp, wc & 0x00ff);
	  break;
	}
    }
  *mp = 0;
}

/* Convert XEmacs string format to the wide-char format expected for wnn. */
void
m2w (Ibyte *mp, w_char *wp)
{
  while (*mp)
    {
      Lisp_Object charset;
      int c1, c2;
      int ch;

      /* @@#### current_buffer dependency */
      buffer_itext_to_charset_codepoint (mp, current_buffer,
					 &charset, &c1, &c2, CONVERR_FAIL);
      INC_IBYTEPTR (mp);
      if (EQ (charset, Vcharset_ascii) ||
	  EQ (charset, Vcharset_latin_jisx0201) ||
	  EQ (charset, Vcharset_katakana_jisx0201))
	ch = c2;
      else if (EQ (charset, Vcharset_japanese_jisx0208) ||
	       EQ (charset, Vcharset_japanese_jisx0208_1978) ||
	       EQ (charset, Vcharset_chinese_gb2312) ||
	       EQ (charset, Vcharset_korean_ksc5601)
	       /* || other 2-byte charsets??? */
	       )
	ch = ((c1 | 0x80) << 8) + (c2 | 0x80);
      else if (EQ (charset, Vcharset_japanese_jisx0212))
	ch = ((c1 | 0x80) << 8) + c2;
      else if (EQ (charset, Fget_charset (Qchinese_sisheng)))
	ch = 0x8e80 | c2;
      else /* Ignore character */
	continue;
      *wp++ = (w_char) ch;
    }
  *wp = 0;
}

void
w2y (w_char *w)
{
  letter		pbuf[5000], ybuf[5000];
  unsigned int		*pin;
  w_char *y;
  int len;

  pin = pbuf;
  y = w;
  while (1)
    {
      if (*w == 0)
	{*pin =0; break;}
      else	       *pin = *w;
      w++; pin++;
    }
  len = cwnn_pzy_yincod (ybuf, pbuf,
			 NILP (Vcwnn_zhuyin) ? CWNN_PINYIN : CWNN_ZHUYIN);
  if (len <= 0)
    return;

  pin = ybuf;
  while (1)
    {
      if (*pin == 0 || len == 0)
	{*y = 0;break;}
      *y = *pin;
      y++; pin++; len--;
    }
}

/* Converts text in the multi-byte locale-specific format returned by some
   WNN functions into XEmacs-internal.  This format appears to be a simple
   MBCS encoding with a single locale, and we could use probably existing
   coding systems to handle it. */

void
c2m (UExtbyte *cp, Ibyte *mp, Lisp_Object charset)
{
  UExtbyte	ch;
  while ((ch = *cp) != 0)
    {
      if (ch & 0x80)
	{
	  mp += charset_codepoint_to_itext (charset, cp[0] & 0x7f,
					    cp[1] & 0x7f, mp,
					    CONVERR_USE_PRIVATE);
	  cp += 2;
	}
      else
	*mp++ = *cp++; /* Guaranteed ASCII */
    }
  *mp = 0;
}

static int
dai_end (int no, int server)
{
  for (no++; no < jl_bun_suu (wnnfns_buf[server])
	       && !jl_dai_top (wnnfns_buf[server], no); no++);
  return (no);
}

static int
yes_or_no (UExtbyte *s)
{
  Ibyte			mbuf[512];
  Lisp_Object		charset;
  int			len;
  int			snum;
  if ((snum  = check_wnn_server_type ()) == -1) return 0;
  charset = charset_wnn_server_type[snum];
  /* if no message found, create file without query */
  /* if (wnn_msg_cat->msg_bd == 0) return 1;*/
  if (*s == 0) return 1;
  c2m (s, mbuf, charset);
  /* truncate "(Y/N)" */
  for (len = 0; (mbuf[len]) && (len < 512); len++);
  for (; (mbuf[len] != '(') && (len > 0); len--);
  {
     Lisp_Object yes, str;
     struct gcpro gcpro1;

     str = make_string (mbuf, len);
     GCPRO1 (str);
     yes = call1 (Qyes_or_no_p, str);
     UNGCPRO;
     if (NILP (yes)) return 0;
     else return (1);
  }
}

static void
puts2 (char *UNUSED (s))
{
#if 0 /* jhod: We don't really need this echoed... */
  Ibyte			mbuf[512];
  Lisp_Object		charset;
  int			snum;
  if ((snum = check_wnn_server_type ()) == -1) return;
  charset = charset_wnn_server_type[snum];
  c2m (s, mbuf, charset);
  message ("%s", mbuf);
#endif
}

int
check_wnn_server_type (void)
{
  if (EQ(Vwnn_server_type, Qjserver))
    {
      return WNNSERVER_J;
    }
  else if (EQ(Vwnn_server_type, Qcserver))
    {
      return WNNSERVER_C;
    }
  /* else if (Vwnn_server_type == Qtserver)
     {
     return WNNSERVER_T;
     } */
  else if (EQ(Vwnn_server_type, Qkserver))
    {
      return WNNSERVER_K;
    }
  else return -1;
}
