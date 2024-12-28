# HTML-Clipboard.ps1

# Pos探す関数群 ----------------------------------------------------------------
function FindStartPos()
{
    param(
        $Text,
        $SearchString
    )

    #$index = $Text.ToLower().IndexOf($SearchString.ToLower())

    $match = Select-String -InputObject $Text.ToLower() -Pattern $SearchString.ToLower()
    $index = $match.Matches[0].Index
    $matchString = $match.Matches[0].Value

    if ($index -eq -1) {
        return 0
    }
    
    # サブ文字列前までの文字列を取得
    $substring = $Text.Substring(0, $index)

    # バイト数計算して返す
    $substringLen = [System.Text.Encoding]::UTF8.GetBytes($substring).Length
    $searchStringLen = [System.Text.Encoding]::UTF8.GetBytes($matchString).Length 

    return $substringLen + $searchStringLen
}

function FindEndPos()
{
    param(
        $Text,
        $SearchString
    )

    $index = $Text.ToLower().IndexOf($SearchString.ToLower())

    if ($index -eq -1) {
        return 0
    }
    
    # サブ文字列前までの文字列を取得
    $substring = $Text.Substring(0, $index)

    # バイト数計算して返す
    $substringLen = [System.Text.Encoding]::UTF8.GetBytes($substring).Length
    return $substringLen
}

function FindStartHTMLPos()
{
    param(
        $Text
    )

    $searchString = "<html>"

    return FindStartPos $Text $searchString
}

function FindEndHTMLPos()
{
    param(
        $Text
    )

    $searchString = "</html>"

    return FindEndPos $Text $searchString
}

function FindStartFragmengPos()
{
    param(
        $Text
    )

    $searchString = "<!--StartFragment-->"

    return FindStartPos $Text $searchString
}

function FindEndFragmengPos()
{
    param(
        $Text
    )

    $searchString = "<!--EndFragment-->"

    return FindEndPos $Text $searchString
}


# ヘッダを作る関数群 -----------------------------------------------------------
# ヘッダを作る関数 - 内部使用
function CreateHeaderInner()
{
    param(
        $HeaderSize,
        $StartHTMLPos,
        $EndHTMLPos,
        $StartFragmengPos,
        $EndFragmengPos
    )

    # 10進 10桁 UINT32.MAX をカバーするため？
    $StartHTML     = "{0:D10}" -f ($HeaderSize + $StartHTMLPos)
    $EndHTML       = "{0:D10}" -f ($HeaderSize + $EndHTMLPos)
    $StartFragment = "{0:D10}" -f ($HeaderSize + $StartFragmengPos)
    $EndFragment   = "{0:D10}" -f ($HeaderSize + $EndFragmengPos)

    $header = @"
Version:0.9
StartHTML:$StartHTML
EndHTML:$EndHTML
StartFragment:$StartFragment
EndFragment:$EndFragment

"@
    
    return $header
}

# header作る関数
# 引数4つ $StartHTMLPos, $EndHTMLPos, $StartFragmengPos, $EndFragmengPos
# ヘッダ抜きのバイト数で受け取って、ヘッダ分を足した形で中身作る
function CreateHeader()
{
    param(
        $StartHTMLPos,     # html開始タグ末尾のバイトを指す
        $EndHTMLPos,       # html終了タグ先頭のバイトを指す
        $StartFragmengPos, # StartFragment開始タグ末尾のバイトを指す
        $EndFragmengPos    # EndFragment終了タグ先頭のバイトを指す
    )
    
    # ヘッダサイズの計算のために一度空のヘッダ作る
    $header = CreateHeaderInner 0 0 0 0 0
    $headerSize = [System.Text.Encoding]::UTF8.GetBytes($header).Length

    $params = @(
        $headerSize,
        $StartHTMLPos,
        $EndHTMLPos,
        $StartFragmengPos,
        $EndFragmengPos
    )
    $header = CreateHeaderInner @params

    return $header
}

# ------------------------------------------------------------------------------

# Fragmentを書き足す関数 -------------------------------------------------------
function InsertFragment()
{
    param(
        $Text
    )

    $start = FindStartPos $Text "<body.*?>"
    $ret = $Text.Insert($start, "<!--StartFragment-->")

    $end = FindEndPos $ret "</body>"
    $ret = $ret.Insert($end, "<!--EndFragment-->")

    return $ret
}

# ------------------------------------------------------------------------------

# Main -------------------------------------------------------------------------
# System.Windows.Formsアセンブリのロード
Add-Type -AssemblyName System.Windows.Forms

# 標準入力から複数行データを読み取る
$reader = [System.IO.StreamReader]::new([System.Console]::OpenStandardInput())
$data = $reader.ReadToEnd()

$html = InsertFragment $data
$startHTMLPos     = FindStartHTMLPos($html)
$endHTMLPos       = FindEndHTMLPos($html)
$startFragmengPos = FindStartFragmengPos($html)
$endFragmengPos   = FindEndFragmengPos($html)

$params = @(
    $startHTMLPos,
    $endHTMLPos,
    $startFragmengPos,
    $endFragmengPos
)
$header = CreateHeader @params

# クリップボードにHTML形式でコピー
[System.Windows.Forms.Clipboard]::SetText(
    $header + $html,
    [System.Windows.Forms.TextDataFormat]::Html
)

# # HTML文字列を作成
# $Text = @"
# <!DOCTYPE html>
# <html>
# <head>
#     <title>Sample HTML</title>
# </head>
# <body>
#     <h1>Hello world</h1>
#     <p>Hello</p>
# </body>
# </html>
# "@

