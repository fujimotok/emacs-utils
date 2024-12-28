# HTML-Clipboard.ps1

# Pos�T���֐��Q ----------------------------------------------------------------
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
    
    # �T�u������O�܂ł̕�������擾
    $substring = $Text.Substring(0, $index)

    # �o�C�g���v�Z���ĕԂ�
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
    
    # �T�u������O�܂ł̕�������擾
    $substring = $Text.Substring(0, $index)

    # �o�C�g���v�Z���ĕԂ�
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


# �w�b�_�����֐��Q -----------------------------------------------------------
# �w�b�_�����֐� - �����g�p
function CreateHeaderInner()
{
    param(
        $HeaderSize,
        $StartHTMLPos,
        $EndHTMLPos,
        $StartFragmengPos,
        $EndFragmengPos
    )

    # 10�i 10�� UINT32.MAX ���J�o�[���邽�߁H
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

# header���֐�
# ����4�� $StartHTMLPos, $EndHTMLPos, $StartFragmengPos, $EndFragmengPos
# �w�b�_�����̃o�C�g���Ŏ󂯎���āA�w�b�_���𑫂����`�Œ��g���
function CreateHeader()
{
    param(
        $StartHTMLPos,     # html�J�n�^�O�����̃o�C�g���w��
        $EndHTMLPos,       # html�I���^�O�擪�̃o�C�g���w��
        $StartFragmengPos, # StartFragment�J�n�^�O�����̃o�C�g���w��
        $EndFragmengPos    # EndFragment�I���^�O�擪�̃o�C�g���w��
    )
    
    # �w�b�_�T�C�Y�̌v�Z�̂��߂Ɉ�x��̃w�b�_���
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

# Fragment�����������֐� -------------------------------------------------------
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
# System.Windows.Forms�A�Z���u���̃��[�h
Add-Type -AssemblyName System.Windows.Forms

# �W�����͂��畡���s�f�[�^��ǂݎ��
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

# �N���b�v�{�[�h��HTML�`���ŃR�s�[
[System.Windows.Forms.Clipboard]::SetText(
    $header + $html,
    [System.Windows.Forms.TextDataFormat]::Html
)

# # HTML��������쐬
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

