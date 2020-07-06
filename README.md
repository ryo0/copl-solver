### なにこれ？という人向けの説明  

「プログラミング言語の基礎概念」という本があります。OCaml（言語）の評価や型システムについて書かれた本です。  
この本ではOCamlサブセットのコードを規則に従って評価していく、という問題がついており、評価が正しいかをチェックできるオンラインジャッジサイトが付いています。  
その問題というのが凶悪で、時間がかかりすぎて自力で解けないようなものが特に後半に出てくるので、公式で自動で答えを出すプログラムを作ることが推奨されています。  
それがこのリポジトリです。  
2020/06/15現在は3章のEvalML1, 4章のEvalML2, 5章のEvalML3, 7章のEvalML4, EvalML5, 8章のTypingML4に対応しています。  

PolyTypingML4はやりかけです。  

TypingML4Resultなどが正しいデータを格納しているのはそのtagリリース時のみで、後方互換性の関係やらで最新のものが正しいとは限りません。  
また、途中まで問題自体は全部解けているもののパーサにバグがあります。8章でパーサのバグがやっとなくなりました。  
