# clmstan Stan開発ガイド

このドキュメントは、clmstanパッケージのStanモデルを修正・拡張する際のガイドラインです。

## ファイル構造

    src/stan/
    ├── clm_base.stan                 # flexible閾値（基本モデル）
    ├── clm_equidistant.stan          # 等間隔閾値
    ├── clm_symmetric.stan            # 対称閾値
    ├── clm_full.stan                 # flexible + リンクパラメータ推論
    ├── clm_equidistant_full.stan     # 等間隔 + リンクパラメータ推論
    ├── clm_symmetric_full.stan       # 対称 + リンクパラメータ推論
    └── functions/
        ├── link_logit.stan           # ロジットリンク
        ├── link_probit.stan          # プロビットリンク
        ├── link_cloglog.stan         # 補対数-対数リンク
        ├── link_loglog.stan          # 対数-対数リンク
        ├── link_cauchit.stan         # コーシーリンク
        ├── link_tlink.stan           # t分布リンク
        ├── link_aranda_ordaz.stan    # Aranda-Ordazリンク
        ├── link_sp.stan              # Symmetric Powerリンク
        ├── link_loggamma.stan        # log-gammaリンク
        ├── link_gev.stan             # GEVリンク
        ├── link_aep.stan             # AEPリンク
        └── clm_common.stan           # 共通dispatcher関数

## `#include` の依存関係

各Stanモデルファイルは以下の順序でインクルードします：

``` stan
functions {
  // 1. まずリンク関数をすべてインクルード
  #include functions/link_logit.stan
  #include functions/link_probit.stan
  #include functions/link_cloglog.stan
  #include functions/link_loglog.stan
  #include functions/link_cauchit.stan
  #include functions/link_tlink.stan
  #include functions/link_aranda_ordaz.stan
  #include functions/link_sp.stan
  #include functions/link_loggamma.stan
  #include functions/link_gev.stan
  #include functions/link_aep.stan

  // 2. 次に共通関数（リンク関数を呼び出すため）
  #include functions/clm_common.stan
}
```

**重要**:
`clm_common.stan`は`link_*.stan`の関数を使用するため、必ずリンク関数の後にインクルードすること。

### 関数の流れ

    モデルファイル (clm_*.stan)
        │
        ├─ #include functions/link_*.stan (11ファイル)
        │     └─ {link}_F(), {link}_logF(), {link}_log1mF()
        │
        └─ #include functions/clm_common.stan
              ├─ unified_F()      ← link_typeに基づいてリンク関数をディスパッチ
              ├─ unified_logF()
              ├─ unified_log1mF()
              ├─ clm_lpmf()       ← 対数尤度計算
              └─ clm_rng()        ← 事後予測サンプリング

## 変更タイプ別ガイド

| 変更内容                   | 影響範囲             | 修正が必要なファイル                                                 |
|----------------------------|----------------------|----------------------------------------------------------------------|
| リンク関数のバグ修正       | 全モデル（自動反映） | `functions/link_*.stan` のみ                                         |
| 新しいリンク関数追加       | 全モデル             | `functions/link_new.stan` + `clm_common.stan` + 全モデルの`#include` |
| `clm_lpmf`/`clm_rng`の修正 | 全モデル（自動反映） | `functions/clm_common.stan` のみ                                     |
| `data`ブロックの変数追加   | 全6モデル            | 各モデル + `R/prepare_data.R`                                        |
| 閾値パラメータの変更       | 該当モデル           | 該当する閾値タイプのモデル                                           |
| リンクパラメータ推論の修正 | `*_full.stan`        | 3つの`_full.stan`モデル                                              |
| 事前分布の変更             | 該当モデル           | 該当モデル + `R/prepare_data.R`                                      |

### 具体例

#### 例1: logitリンクのバグ修正

    修正: functions/link_logit.stan
    → 全モデルに自動的に反映（#includeのため）

#### 例2: dataブロックに新しい変数追加

    修正が必要:
    1. clm_base.stan
    2. clm_equidistant.stan
    3. clm_symmetric.stan
    4. clm_full.stan
    5. clm_equidistant_full.stan
    6. clm_symmetric_full.stan
    7. R/prepare_data.R（Stan dataを生成する関数）

## instantiateパッケージの注意事項

### モデル検出の仕組み

`instantiate`は`src/stan/`ディレクトリ内の`.stan`ファイルをモデルとしてコンパイルします。

``` r
# install.libs.R での設定
model_files <- fs::dir_ls(bin_stan, regexp = "[.]stan$", recurse = FALSE)
```

**重要**:
`recurse = FALSE`により、`functions/`サブディレクトリ内のファイルはモデルとしてコンパイルされません。これにより、インクルード専用ファイルが誤ってスタンドアロンモデルとして処理されることを防いでいます。

### 新しいモデルを追加する場合

1.  `src/stan/`ディレクトリ直下に`.stan`ファイルを配置
2.  `functions/`内にはインクルード専用ファイルのみ配置
3.  パッケージ再ビルドで自動的に検出・コンパイル

## 整合性テスト

`tests/testthat/test-model-consistency.R`が以下を自動的にチェックします：

- 全モデルが`clm_common.stan`をインクルードしているか
- 全モデルに`RELATED FILES`コメントがあるか
- `data`ブロックの共通変数が一致しているか
- フルモデル（`_full.stan`）が推論フラグを持っているか

テスト実行：

``` r
devtools::test(filter = "model-consistency")
```

## R側との対応

| Stanファイル                | R関数                                                                           |
|-----------------------------|---------------------------------------------------------------------------------|
| `clm_base.stan`             | [`prepare_stan_data()`](reference/prepare_stan_data.md)                         |
| `clm_equidistant.stan`      | [`prepare_stan_data_equidistant()`](reference/prepare_stan_data_equidistant.md) |
| `clm_symmetric.stan`        | [`prepare_stan_data_symmetric()`](reference/prepare_stan_data_symmetric.md)     |
| `clm_full.stan`             | [`prepare_stan_data_full()`](reference/prepare_stan_data_full.md)               |
| `clm_equidistant_full.stan` | `prepare_stan_data_equidistant_full()`                                          |
| `clm_symmetric_full.stan`   | `prepare_stan_data_symmetric_full()`                                            |

モデル選択は[`get_model_name()`](reference/get_model_name.md)関数で行われます：

``` r
get_model_name(threshold = "flexible", full = FALSE)
# → "clm_base"

get_model_name(threshold = "equidistant", full = TRUE)
# → "clm_equidistant_full"
```

## チェックリスト

Stanモデルを修正する前に：

変更の影響範囲を上の表で確認

影響を受ける全ファイルをリストアップ

各ファイル内の`RELATED FILES`コメントを参照

修正後に`devtools::test()`を実行して整合性を確認
