# eigenmat

![image](image.png)

## 概要
- Gauche で、Eigen ライブラリ を使って行列の高速演算を行うためのモジュールです。  
  Eigen は、C++ で書かれた線形代数用のライブラリです ( http://eigen.tuxfamily.org )。  
  現状、本モジュールは、標準の gauhce.array モジュールにおける  
  2次元の f64array のみ演算が可能です。


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
   (Eigen は、C++ のヘッダーファイルのみのライブラリであるため、  
   インストールはコピーするだけです)

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
   
   (注意) もし、MSYS2/MinGW-w64 の pacman でインストールした Eigen を使用したい場合には、  
   上記の ./configure の部分を以下のように変更してください。
   ```
     ./configure CPPFLAGS=`pkg-config --cflags eigen3`
   ```

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
- 使用可能な手続きを、以下に示します。  
  (現状、本モジュールは、標準の gauhce.array モジュールにおける  
  2次元の f64array のみ演算が可能です)

  - `(eigen-array-cache-on)`  
    `(eigen-array-cache-off)`  
    行列のキャッシュの使用(ON)/未使用(OFF)を設定します。デフォルトは使用(ON)です。  
    (行列のキャッシュについては、後述の make-eigen-array の説明を参照ください)

  - `(make-eigen-array ns ne ms me [maybe-init])`  
    行列 (2次元のf64array) を生成します。  
    ns ne ms me には、行列の shape を指定します。  
    例えば、2x3 の行列を生成する場合には、  
    `(make-eigen-array 0 2 0 3)` のように指定します。  
    maybe-init には要素の初期値を指定します。  
    maybe-init は省略可能です。省略した場合は 0 を指定したことになります。  
    本手続きは、生成した行列をキャッシュに保存します。  
    そして、同一サイズの行列の生成については、キャッシュのコピーを返すことで高速化をはかります。  
    もし、メモリ使用量やマルチスレッド等の関係で、本処理が不要な場合には、  
    事前に `(eigen-array-cache-off)` を呼び出してください。  
    (本手続きは、他の手続きの内部でも使用しています)

  - `(make-eigen-array-same-shape A [maybe-init])`  
    行列A と同じ shape の行列 (2次元のf64array) を生成します。  
    maybe-init には要素の初期値を指定します。  
    maybe-init は省略可能です。省略した場合は 0 を指定したことになります。

  - `(eigen-array ns ne ms me . inits)`  
    初期化データを指定して、行列 (2次元のf64array) を生成します。  
    ns ne ms me には、行列の shape を指定します。  
    inits には、行列の要素の数だけ初期化データを並べます。  
    例えば、2x3 の行列を生成する場合には、  
    `(eigen-array 0 2 0 3 1 2 3 4 5 6)` のように指定します。

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
    `(eigen-array-add! C A B)`  
    行列A と 行列B の和を計算して返します。  
    `!` がついたものは、結果を 行列C に格納して返します (行列C は変更されます)。

  - `(eigen-array-add A r)`  
    `(eigen-array-add! B A r)`  
    行列A と 実数r の和を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-sub A B)`  
    `(eigen-array-sub! C A B)`  
    行列A と 行列B の差を計算して返します。  
    `!` がついたものは、結果を 行列C に格納して返します (行列C は変更されます)。

  - `(eigen-array-sub A r)`  
    `(eigen-array-sub! B A r)`  
    行列A と 実数r の差を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-mul A B)`  
    `(eigen-array-mul! C A B)`  
    行列A と 行列B の積を計算して返します。  
    `!` がついたものは、結果を 行列C に格納して返します (行列C は変更されます)。

  - `(eigen-array-mul-elements A B)`  
    `(eigen-array-mul-elements! C A B)`  
    行列A と 行列B の要素の積を計算して返します。  
    `!` がついたものは、結果を 行列C に格納して返します (行列C は変更されます)。

  - `(eigen-array-mul-elements A r)`  
    `(eigen-array-mul-elements! B A r)`  
    行列A の要素と 実数r の積を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-div A B)`  
    `(eigen-array-div! C A B)`  
    行列A の要素を 行列B の要素で割り算した結果を返します。  
    `!` がついたものは、結果を 行列C に格納して返します (行列C は変更されます)。

  - `(eigen-array-div A r)`  
    `(eigen-array-div! B A r)`  
    行列A の要素を 実数r で割り算した結果を返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-pow A r)`  
    `(eigen-array-pow! B A r)`  
    行列A の要素を 実数r を用いてr乗した結果を返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-exp A)`  
    `(eigen-array-exp! B A)`  
    行列A の要素を指数として、自然対数の底eのべき乗を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-log A)`  
    `(eigen-array-log! B A)`  
    行列A の要素に対して、自然対数を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-sinh A)`  
    `(eigen-array-sinh! B A)`  
    行列A の要素に対して、sinh を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-cosh A)`  
    `(eigen-array-cosh! B A)`  
    行列A の要素に対して、cosh を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-tanh A)`  
    `(eigen-array-tanh! B A)`  
    行列A の要素に対して、tanh を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-sigmoid A)`  
    `(eigen-array-sigmoid! B A)`  
    行列A の要素に対して、シグモイド関数 を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-relu A)`  
    `(eigen-array-relu! B A)`  
    行列A の要素に対して、ReLU関数 を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-step A)`  
    `(eigen-array-step! B A)`  
    行列A の要素に対して、ステップ関数 を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-sum A)`  
    行列A の要素の和を計算して返します。

  - `(eigen-array-min A)`  
    行列A の要素の最小値を返します。

  - `(eigen-array-max A)`  
    行列A の要素の最大値を返します。

  - `(eigen-array-mean A)`  
    行列A の要素の平均値を計算して返します。

  - `(eigen-array-trace A)`  
    行列A のトレースを計算して返します。

  - `(eigen-array-determinant A)`  
    行列A の行列式を計算して返します。

  - `(eigen-array-identity n m)`  
    `(eigen-array-identity! A)`  
    サイズが n x m の単位行列を返します。  
    `!` がついたものは、行列A を (サイズは変えずに) 単位行列にして返します (行列A は変更されます)。

  - `(eigen-array-transpose A)`  
    `(eigen-array-transpose! B A)`  
    行列A の転置行列を返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-inverse A)`  
    `(eigen-array-inverse! B A)`  
    行列A の逆行列を計算して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-solve A B)`  
    `(eigen-array-solve! X A B)`  
    AX=B となる 行列X を計算して返します。  
    `!` がついたものは、結果を 行列X に格納して返します (行列X は変更されます)。

  - `(eigen-array-row A i)`  
    `(eigen-array-row! B A i)`  
    行列A から i 行を抜き出して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-col A j)`  
    `(eigen-array-col! B A j)`  
    行列A から j 列を抜き出して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-block A i j p q)`  
    `(eigen-array-block! B A i j p q)`  
    行列A から 開始位置が (i,j) でサイズが (p,q) の行列を抜き出して返します。  
    `!` がついたものは、結果を 行列B に格納して返します (行列B は変更されます)。

  - `(eigen-array-block-copy A i1 j1 p q B i2 j2)`  
    `(eigen-array-block-copy! C A i1 j1 p q B i2 j2)`  
    行列A から 開始位置が (i1,j1) でサイズが (p,q) の行列を抜き出して、  
    行列B の (i2,j2) の位置にコピーしたものを返します。  
    (行列B は変更されません。戻り値を使用してください)  
    `!` がついたものは、結果を 行列C に格納して返します (行列C は変更されます)。


## 注意事項
1. 本モジュールは、標準の gauche.array モジュールの内部情報を使用しています。  
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
  - Gauche v0.9.7
  - Gauche v0.9.6
- ライブラリ
  - Eigen v3.3.7

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
- 2019-2-22  v1.10 eigen-array-blockを追加
- 2019-2-23  v1.11 eigen-array-blockのコピー機能を追加
- 2019-2-23  v1.12 eigen-array-div,eigen-array-pow,eigen-array-sum,  
  eigen-array-min,eigen-array-max,eigen-array-mean,  
  eigen-array-trace,eigen-array-transposeを追加
- 2019-2-26  v1.13 eigen-array-mul-elements,eigen-array-exp,  
  eigen-array-log,eigen-array-sigmoidを追加
- 2019-2-26  v1.14 (eigen-array-mul A r) を (eigen-array-mul-elements A r) に変更
- 2019-2-26  v1.15 可能なときは行列の生成にコピーを使用するようにした  
  eigen-array-inverseとeigen-array-solveの結果の行列サイズ修正
- 2019-2-27  v1.16 eigen-make-array,eigen-array,eigen-array-reluを追加  
  生成した行列のキャッシュを保存するようにした
- 2019-2-27  v1.17 eigen-array-stepを追加
- 2019-2-28  v1.18 プログラムの整理等
- 2019-3-1   v1.19 use-eigen-array-cacheを削除  
  eigen-array-cahce-on,eigen-array-cahce-off,  
  eigen-array-row,eigen-array-colを追加
- 2019-3-1   v1.20 行列のチェック見直し
- 2019-3-1   v1.21 Cの数値の記述見直し
- 2019-3-2   v1.22 行列のチェック見直し等
- 2019-3-3   v1.23 演算の破壊的変更版を追加
- 2019-3-4   v1.24 行列のキャッシュミス修正  
  eigen-array-nearly-zero?のミス修正(EigenのisMuchSmallerThan関数の引数を間違えていた)
- 2019-3-11  v1.25 eigen-array-transpose!,eigen-array-inverse!,eigen-array-solve!,  
  eigen-array-row!,eigen-array-col!,eigen-array-block!を追加
- 2019-3-19  v1.26 行列の生成処理見直し。eigen-make-array-same-shapeを追加
- 2019-3-19  v1.27 不要処理削除
- 2019-3-19  v1.28 手続き名変更(eigen-make-array → make-eigen-array 等。旧名称も使用可能)
- 2019-3-21  v1.29 行列の情報取得をマクロ化
- 2019-3-22  v1.30 行列の要素の割り算を追加
- 2019-3-22  v1.31 エラー処理修正(eigen-array-determinant,eigen-array-inverse  
  eigen-array-inverse!,eigen-array-solve,eigen-array-solve!)
- 2019-3-23  v1.32 手続き名変更(eigen-array-block → eigen-array-block-copy)
- 2019-4-7   v1.33 引数チェック強化(make-eigen-array,make-eigen-array-same-shape)
- 2019-4-13  v1.34 eigen-array-sinh,eigen-array-sinh!,  
  eigen-array-cosh,eigen-array-cosh!,  
  eigen-array-tanh,eigen-array-tanh!を追加
- 2019-4-14  v1.35 eigen-array-identity,eigen-array-identity!を追加
- 2019-4-16  v1.36 Cの処理見直し(データコピー削減)
- 2019-4-18  v1.37 変数のmとnを交換(m=行数,n=列数に統一した(教科書の記述に合わせた))
- 2019-4-18  v1.38 v1.37の変更を元に戻した(n=行数,m=列数という流儀もあるもよう。。。)


(2019-4-18)
