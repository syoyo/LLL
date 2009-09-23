=================
LLL language spec
=================

This document is written in Japanese. We will provide English version at some point.
Character encoding of this document is UTF-8.

Introduction
============

LLL(Lucille Light transport Language) は, レイトレーシング世代のシェーディング言語です. 既存のシェーディング言語の多くが, ラスタライザグラフィックスや局所照明モデルを仮定して設計されているため, レイトレーシンググラフィックスにてこれら既存のシェーディング言語を利用しようとすると多くの不整合な点が生じていました.
LLL では, レイトレーシング世代に注力してシェーディング言語を設計することで, 自然でかつパワフルなシェーダシステムを提供します.

Features
--------

LLL の特徴は以下の通りです.

* レイトレーシングアルゴリズムに適した言語体系.
* 実行が高速であること. 言語仕様は, JIT コンパイルが容易であり, SIMD の恩恵を得やすく, スレッド実行しやすい(データ依存関係が低い)ように設計しています.
* 関数型言語. LLL は関数型言語に近い言語構造になります. 関数型にすることで, 副作用を少なくし、シェーダグラフとの連携を容易にします.
* 型推論の導入. 型は推論されます. 本当に必要なときのみ型を指定することで, コードのタイプ量を減らします.
* ベクトル型. LLL ではプリミティブな型として、ベクトル型を導入します. ベクトル型は SIMD 演算にマップすることで、高速な実行が期待されます.


Spec
====

T.B.W.


References
==========

* Regiment. http://regiment.us/regiment_webpage/index.html
* Faust, signal processing language. http://faust.grame.fr/
