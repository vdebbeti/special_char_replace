/* ---------------------------------------------------------------
   clean_special_chars.sas

   Standalone SAS program that does the same job as the
   "Special Character Replacer" Shiny app: walks every character
   variable in a dataset, replaces common non-ASCII characters
   with ASCII equivalents, optionally strips ASCII control
   characters (CR/LF/TAB/etc.), and writes a change log.

   Requires a UTF-8 SAS session (ENCODING=UTF-8). Check with:
       proc options option=encoding; run;
   Unicode code points are addressed via PRX \x{hhhh} syntax.

   Usage:
       %clean_special_chars(
           indata        = mylib.raw,
           outdata       = mylib.clean,
           report        = work.change_log,
           strip_control = Y);

   Notes:
   - Character variables are not expanded: SAS keeps the original
     byte length. Since ASCII replacements are usually shorter
     than the UTF-8 originals, truncation is rarely an issue.
   - If a variable is too short, lengthen it in the input
     dataset before running this macro.
   - The codepoint column is left blank here; if you need it,
     post-process the change log with a helper that walks each
     string byte-by-byte.
   --------------------------------------------------------------- */

%macro clean_special_chars(indata=, outdata=, report=work.change_log,
                           strip_control=N);

  data &outdata (drop=_i _orig _v)
       &report  (keep=variable row original codepoint replacement);

    set &indata;

    /* Scratch variables -- large enough to hold any character
       field you are likely to process. Adjust if you have bigger
       free-text columns. */
    length _orig      $32767
           _v         $32767
           variable    $32
           original    $500
           codepoint    $40
           replacement $500;

    _row = _N_;

    /* Iterate every character variable in the current row */
    array _c {*} _character_;
    do _i = 1 to dim(_c);
      _orig = _c[_i];
      _v    = _orig;

      /* ---------- Curly quotes -> straight ---------- */
      _v = prxchange("s/[\x{2018}\x{2019}\x{201A}\x{201B}]/'/", -1, _v);
      _v = prxchange('s/[\x{201C}\x{201D}\x{201E}\x{201F}]/"/', -1, _v);

      /* ---------- Dashes and minus ---------- */
      _v = prxchange('s/[\x{2013}\x{2014}\x{2212}\x{2015}]/-/', -1, _v);

      /* ---------- Ellipsis, bullet ---------- */
      _v = prxchange('s/\x{2026}/.../',   -1, _v);
      _v = prxchange('s/\x{2022}/*/',     -1, _v);

      /* ---------- Non-breaking / fancy spaces ---------- */
      _v = prxchange('s/[\x{00A0}\x{2002}\x{2003}\x{2009}]/ /', -1, _v);
      _v = prxchange('s/\x{200B}//',      -1, _v);

      /* ---------- Angle quotes / guillemets ---------- */
      _v = prxchange('s/\x{2039}/</',     -1, _v);
      _v = prxchange('s/\x{203A}/>/',     -1, _v);
      _v = prxchange('s/\x{00AB}/<</',    -1, _v);
      _v = prxchange('s/\x{00BB}/>>/',    -1, _v);

      /* ---------- Symbols ---------- */
      _v = prxchange('s/\x{00AE}/(R)/',   -1, _v);
      _v = prxchange('s/\x{00A9}/(C)/',   -1, _v);
      _v = prxchange('s/\x{2122}/(TM)/',  -1, _v);
      _v = prxchange('s/\x{00B0}/ deg/',  -1, _v);
      _v = prxchange('s/\x{00B1}/+\/-/',  -1, _v);
      _v = prxchange('s/\x{00B2}/2/',     -1, _v);
      _v = prxchange('s/\x{00B3}/3/',     -1, _v);
      _v = prxchange('s/\x{00BC}/1\/4/',  -1, _v);
      _v = prxchange('s/\x{00BD}/1\/2/',  -1, _v);
      _v = prxchange('s/\x{00BE}/3\/4/',  -1, _v);
      _v = prxchange('s/\x{00B5}/u/',     -1, _v);
      _v = prxchange('s/\x{00D7}/x/',     -1, _v);
      _v = prxchange('s/\x{00F7}/\//',    -1, _v);

      /* ---------- Currency ---------- */
      _v = prxchange('s/\x{20AC}/EUR/',   -1, _v);
      _v = prxchange('s/\x{00A3}/GBP/',   -1, _v);
      _v = prxchange('s/\x{00A5}/JPY/',   -1, _v);
      _v = prxchange('s/\x{00A2}/c/',     -1, _v);
      _v = prxchange('s/\x{20B9}/INR/',   -1, _v);

      /* ---------- Common Latin-1 accented letters -> ASCII ---------- */
      _v = prxchange('s/[\x{00E0}-\x{00E5}]/a/',         -1, _v);
      _v = prxchange('s/[\x{00C0}-\x{00C5}]/A/',         -1, _v);
      _v = prxchange('s/\x{00E6}/ae/',                   -1, _v);
      _v = prxchange('s/\x{00C6}/AE/',                   -1, _v);
      _v = prxchange('s/\x{00E7}/c/',                    -1, _v);
      _v = prxchange('s/\x{00C7}/C/',                    -1, _v);
      _v = prxchange('s/[\x{00E8}-\x{00EB}]/e/',         -1, _v);
      _v = prxchange('s/[\x{00C8}-\x{00CB}]/E/',         -1, _v);
      _v = prxchange('s/[\x{00EC}-\x{00EF}]/i/',         -1, _v);
      _v = prxchange('s/[\x{00CC}-\x{00CF}]/I/',         -1, _v);
      _v = prxchange('s/\x{00F1}/n/',                    -1, _v);
      _v = prxchange('s/\x{00D1}/N/',                    -1, _v);
      _v = prxchange('s/[\x{00F2}-\x{00F6}\x{00F8}]/o/', -1, _v);
      _v = prxchange('s/[\x{00D2}-\x{00D6}\x{00D8}]/O/', -1, _v);
      _v = prxchange('s/[\x{00F9}-\x{00FC}]/u/',         -1, _v);
      _v = prxchange('s/[\x{00D9}-\x{00DC}]/U/',         -1, _v);
      _v = prxchange('s/\x{00FD}/y/',                    -1, _v);
      _v = prxchange('s/\x{00FF}/y/',                    -1, _v);
      _v = prxchange('s/\x{00DD}/Y/',                    -1, _v);
      _v = prxchange('s/\x{00DF}/ss/',                   -1, _v);

      %if %upcase(&strip_control) = Y %then %do;
        /* ---------- ASCII control characters ---------- */
        _v = prxchange('s/\x{0009}/ /', -1, _v);  /* TAB -> space */
        _v = prxchange('s/\x{000A}/ /', -1, _v);  /* LF  -> space */
        _v = prxchange('s/[\x{0000}-\x{0008}\x{000B}-\x{000D}\x{000E}-\x{001F}\x{007F}]//',
                       -1, _v);
      %end;

      /* ---------- Drop anything still outside printable ASCII ---------- */
      _v = prxchange('s/[^\x{0020}-\x{007E}]//', -1, _v);

      /* ---------- Log the change ---------- */
      if _orig ne _v then do;
        variable    = vname(_c[_i]);
        row         = _row;
        original    = _orig;
        replacement = _v;
        codepoint   = "";
        output &report;
      end;

      /* Write the cleaned value back into the array slot */
      _c[_i] = _v;
    end;

    output &outdata;
  run;

  proc print data=&report (obs=20) label;
    title "Change report (first 20 rows)";
  run;
  title;

%mend clean_special_chars;

/* ---------------- Example ----------------
libname mylib "/path/to/data";

%clean_special_chars(
    indata        = mylib.sample_specialchars,
    outdata       = mylib.sample_clean,
    report        = work.change_log,
    strip_control = Y);
*/
