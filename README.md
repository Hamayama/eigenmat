# eigenmat

![image](image.png)

## 概要
- Gauche で、Eigen ライブラリ を使って行列の高速演算を行うためのモジュールです。  
  Eigen は、C++ で書かれた線形代数用のライブラリです ( http://eigen.tuxfamily.org )。  
  現状、本モジュールは、標準の gauhce.array モジュールにおける  
  `<f64array>` クラスのごく一部の演算にのみ対応しています。


## インストール方法
1. Gauche のインストール  
   事前に Gauche がインストールされている必要があります。  
   Windows の場合は、以下のページに Windows用バイナリインストーラ があるので  
   インストールを実施ください。  
   http://practical-scheme.net/gauche/download-j.html  
   (すでにインストール済みであれば本手順は不要です)

2. 開発環境のインストール  
   C と C++ の開発環境が必要です。  
   Windows の場合は、以下のページを参考に、MSYS2/MinGW-w64 (64bit/32bit) の  
   開発環境をインストールしてください。  
   https://gist.github.com/Hamayama/eb4b4824ada3ac71beee0c9bb5fa546d  
   (すでにインストール済みであれば本手順は不要です)

3. Eigen ライブラリ のインストール  
   Eigen のホームページ ( http://eigen.tuxfamily.org ) から、  
   ソース (zipファイル等) を取得して、適当なフォルダに展開してください。  
   そして、中の Eigen フォルダを、  
   開発環境のインクルードフォルダにコピーしてください。  
   Windows の場合は、  
   C:\msys64\mingw64\include にコピーしてください。  
   (32bit環境の場合には、64 の部分を 32 に読み替えてください)

4. ファイルのダウンロード  
   本サイト ( https://github.com/Hamayama/eigenmat ) のソースを、  
   (Download Zip ボタン等で) ダウンロードして、作業用のフォルダに展開してください。  
   例えば、作業用のフォルダを c:\work とすると、  
   c:\work\eigenmat の下にファイル一式が配置されるように展開してください。  
   (注意) 作業用フォルダのパスには、空白を入れないようにしてください。

5. コンパイルとインストール  
   展開したフォルダで、./configure と make install を実行して、  
   インストールを実施ください。  
   Windows の場合は、以下のように実行してください。  
   ＜MSYS2/MinGW-w64 (64bit) 環境の場合＞  
   プログラムメニューから MSYS2 の MinGW 64bit Shell を起動して、以下のコマンドを実行します。  
   ＜MSYS2/MinGW-w64 (32bit) 環境の場合＞  
   プログラムメニューから MSYS2 の MinGW 32bit Shell を起動して、以下のコマンドを実行します。  
   ( c:\work にソースを展開した場合)
   ```
     cd /c/work/eigenmat
     ./configure   # Makefile等を生成します
     make          # コンパイルを実行します
     make install  # Gaucheのライブラリフォルダにインストールします
     make check    # テストを実行します
   ```
   (注意) Windows の環境によっては、make install を実行すると  
   「*** ERROR: mkstemp failed」というエラーが発生します。  
   このエラーは、インストール先のフォルダに書き込み権限がないとき等に発生します。  
   その場合には、プログラムメニューからの開発環境の起動時に右クリックして、  
   「管理者として実行」を選択してください。  
   そして再度上記のコマンドを実行してください。

- 以上です。


## 使い方
- 基本的な使い方は、以下のようになります。
  ```
    (use gauche.array)        ; 標準の行列演算モジュールをロードします。
    (use eigenmat)            ; 本モジュールをロードします。
    (define A (f64array (shape 0 2 0 2) 1 2 3 4)) ; 2x2 の 行列A を作成します。
    (define B (f64array (shape 0 2 0 2) 5 6 7 8)) ; 2x2 の 行列B を作成します。
    (define C (eigen-array-mul A B)) ; 行列A と 行列B の積を計算して、行列C に格納します。
    (print C)                 ; 行列C の内容を表示します。
    (print (array-ref C 0 0)) ; 行列C の左上の要素の値を表示します。
    (print (array-ref C 0 1)) ; 行列C の右上の要素の値を表示します。
  ```
- 現状、本モジュールは、標準の gauhce.array モジュールにおける  
  `<f64array>` クラスの以下の演算にのみ対応しています。

  - `(eigen-array-nearly=? A B [precision])`  
    行列A と 行列B の各要素がほぼ等しければ #t を返します。  
    そうでなければ #f を返します。  
    precision には、比較の精度を示す値を指定します。  
    (precision の定義については、Eigen ライブラリ の isApprox メソッドの説明を参照してください)  
    precision は省略可能です。省略した場合は 1e-12 を指定したことになります。  
    注意) この手続きには、「A と B のどちらかが "誤差のないゼロ行列" のとき、  
    A と B の両方が "誤差のないゼロ行列" の場合にのみ #t を返す」という特徴があります。  
    このため、誤差を許す「"誤差のないゼロ行列" との比較」を行いたい場合には、  
    後述の eigen-array-nearly-zero? を使用してください。

  - `(eigen-array-nearly-zero? A [precision])`  
    行列A が ゼロ行列 に近ければ #t を返します。  
    そうでなければ #f を返します。  
    precision には、比較の精度を示す値を指定します。  
    (precision の定義については、Eigen ライブラリ の isMuchSmallerThan メソッドの説明を参照してください)  
    precision は省略可能です。省略した場合は 1e-12 を指定したことになります。

  - `(eigen-array-add A B)`  
    行列A と 行列B の和を計算して返します。

  - `(eigen-array-add A r)`  
    行列A と 実数r の和を計算して返します。

  - `(eigen-array-sub A B)`  
    行列A と 行列B の差を計算して返します。

  - `(eigen-array-sub A r)`  
    行列A と 実数r の差を計算して返します。

  - `(eigen-array-mul A B)`  
    行列A と 行列B の積を計算して返します。

  - `(eigen-array-mul A r)`  
    行列A と 実数r の積を計算して返します。

  - `(eigen-array-determinant A)`  
    行列A の行列式を計算して返します。

  - `(eigen-array-inverse A)`  
    行列A の逆行列を計算して返します。

  - `(eigen-array-solve A B)`  
    AX=B となる 行列X を計算して返します。


## 注意事項
1. 本モジュールは、標準の gauche.array モジュールにおける  
   `<f64array>` クラスの内部情報 (backing-storage スロット) を使用しています。  
   このため、Gauche の将来の変更で動作しなくなる可能性があります。


## 参考情報
1. Gauche-gl-examples ( https://github.com/Hamayama/Gauche-gl-examples )  
   の pendulum.scm は、本モジュールが存在すれば使用するようになっています。  
   (振り子の数 `*N*` を大きくすると、高速化の効果が分かりやすいです)


## 環境等
- OS
  - Windows 8.1 (64bit)
- 環境
  - MSYS2/MinGW-w64 (64bit/32bit) (gcc version 7.3.0 (Rev2, Built by MSYS2 project))
- 言語
  - Gauche v0.9.6
- ライブラリ
  - Eigen v3.3.5


## 履歴
- 2018-8-13  v1.00 (初版)
- 2018-8-13  v1.01 コメント修正のみ
- 2018-8-14  v1.02 行列式のチェックを削除
- 2018-8-14  v1.03 内部関数名見直し等
- 2018-8-14  v1.04 ヘッダファイルの読み込み順見直し
- 2018-8-18  v1.05 eigen-array-nearly-zero?,eigen-array-add,eigen-array-subを追加  
  eigen-array-nearly=?の精度の初期値を変更(Eigenライブラリに合わせた(1e-4 → 1e-12))
- 2018-8-18  v1.06 0x0の行列で実行時エラーになる件の対策(eigen-array-inverse,eigen-array-solve)
- 2018-8-18  v1.07 配列の添字ミス修正等
- 2018-8-20  v1.08 eigen-array-add-scalar,eigen-array-sub-scalar,eigen-array-mul-scalarを追加
- 2018-8-20  v1.09 v1.08の追加分を削除してジェネリックファンクションで対応


(2018-8-20)
