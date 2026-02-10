#include <QApplication>
#include <QWidget>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QLabel>
#include <QPainter>
#include <QTimer>
#include <QElapsedTimer>
#include <QVector>
#include <QFile>
#include <QXmlStreamReader>
#include <cmath>
#include <QDebug>
#include <QPainterPath>
#include <QFileDialog>
#include <QMessageBox>
#include <QStaticText>
#include <QLinearGradient>
#include <regex>
#include <utility>

// --- EXPRTK HEADER ---
#include "exprtk.hpp"

// --- LMMS CONSTANTS ---
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define F_PI (float)M_PI
#define F_2PI (float)(2.0 * M_PI)

// --- RANDOM DATA (Full Array from LMMS Source) ---
static const unsigned int random_data[257] = {
    0xd76a33ec, 0x4a767724, 0xb34ebd08, 0xf4024196, 0x17b426e2, 0x8dc6389a, 0x1b5dcb93, 0xa771bd3f,
    0x078d502e, 0x8980988a, 0x1f64f846, 0xb5b48ed7, 0xf0742cfb, 0xe7c66303, 0xc9472876, 0x6c7494a5,
    0x5c2203a1, 0x23986344, 0x7d344fa0, 0x4f39474a, 0x28ac8b2b, 0x10f779b2, 0x6e79e659, 0x32e44c52,
    0xf790aa55, 0x98b05083, 0xb5d44f1c, 0xe553da04, 0xa884c6d2, 0x43274953, 0xbcb57404, 0x43f7d32a,
    0xf1890f8b, 0x019f4dce, 0x5c4ede33, 0x2dec1a7e, 0x0f3eab09, 0x2197c93c, 0xae933f42, 0x80d4b111,
    0x6e5bd30a, 0x17139c70, 0xe15f7eb0, 0x1798f893, 0xe1c6be1c, 0xe21edf9b, 0x4702e081, 0x8a2cb85a,
    0xbf3c1f15, 0x147f4685, 0x9221d731, 0x3c7580f3, 0xc1c08498, 0x8e198b35, 0xf821c15a, 0x4d3cd2d4,
    0xad89a3b7, 0xd48f915f, 0xcaf893f0, 0xa64a4b8e, 0x20715f54, 0x1ba4de0a, 0x17ac6e91, 0xd82ea8c0,
    0x638a0ba5, 0xe7a76c0f, 0x486c5476, 0x334bbd0a, 0xffe29c55, 0x7247efaf, 0x15f98e83, 0x7a4a79ac,
    0x350cd175, 0xc7107908, 0xa85c67f7, 0x9c5002c4, 0x3cf27d2c, 0x314d8450, 0x05552886, 0x87a73642,
    0x827832e4, 0x9412cc67, 0x261979e6, 0xb31da27f, 0x3e6bbafb, 0x663f1968, 0xd84274e2, 0xdd91d982,
    0xd25c4805, 0x9567f860, 0xab99675c, 0x2254733b, 0x18799dd7, 0xee328916, 0xb9419a1b, 0x01b7a66f,
    0xbcdc05e1, 0x788de4ae, 0x366e77cf, 0x81a1ebd2, 0x97be859a, 0x17d4b533, 0x22dab3a9, 0xc99871ea,
    0xc7502c91, 0x4474b65f, 0x655d059d, 0x0ddc1348, 0x8325909b, 0x4873c155, 0x9fa30438, 0x7250b7a8,
    0x90db2715, 0xf65e1cef, 0x41b74cf4, 0x38fba01c, 0xe9eefb40, 0x9e5524ea, 0x1d3fc077, 0x04ec39db,
    0x1c0d501c, 0xb93f26d9, 0xf9f910b9, 0x806fce99, 0x5691ffdf, 0x1e63b27a, 0xf2035d42, 0xd3218a0b,
    0x12eae6db, 0xeba372a9, 0x6f975260, 0xc514ae91, 0xebddb8ad, 0xc53207c0, 0xdbda57dc, 0x8fb38ef4,
    0xfaa4f1bc, 0x40caf49f, 0xcb394b41, 0x424fc698, 0xb79a9ece, 0x331202d6, 0xc604ed4d, 0x5e85819f,
    0x67222eda, 0xd976ba71, 0x7d083ec6, 0x040c819e, 0xc762c934, 0xa6684333, 0x2eaccc54, 0x69dc04b9,
    0x0499cf36, 0x6351f438, 0x6db2dc34, 0x787ae036, 0x11b5c6ac, 0x552b7227, 0x32a4c993, 0xf7f4c49d,
    0x7ac9e2d9, 0xf3d32020, 0x4ff01f89, 0x6f0e60bb, 0x3c6ed445, 0x7ca01986, 0x96901ecf, 0xe10df188,
    0x62a6da6d, 0x8deee09f, 0x5347cb66, 0x5249f452, 0x22704d4d, 0x6221555f, 0x6aa0ea90, 0xe1f7bae3,
    0xd106626f, 0x6365a9db, 0x1989bb81, 0xfc2daa73, 0x303c60b3, 0xcd867baa, 0x7c5787c2, 0x60082b30,
    0xa68d3a81, 0x15a10f5d, 0x81b21c8a, 0x4bfb82e2, 0xff01c176, 0xff3c8b65, 0x8cc6bd29, 0xc678d6ff,
    0x99b86508, 0x3c47e314, 0x766ecc05, 0xba186cb0, 0x42f57199, 0x5ef524f4, 0xb8419750, 0x6ae2a9d0,
    0x291eaa18, 0x4e64b189, 0x506eb1d3, 0x78361d46, 0x6a2fcb7e, 0xbc0a46de, 0xb557badf, 0xad3de958,
    0xa2901279, 0x491decbf, 0x257383df, 0x94dd19d1, 0xd0cfbbe2, 0x9063d36d, 0x81e44c3b, 0x973e9cc9,
    0xfbe34690, 0x4eee3034, 0x1c413676, 0xf6735b8f, 0xf2991aca, 0x0ec85159, 0x6ce00ade, 0xad49de57,
    0x025edf30, 0x42722b67, 0x30cfa6b2, 0x32df8676, 0x387d4500, 0x97fa67fd, 0x027c994a, 0x77c71d0c,
    0x478eb75a, 0x898370a6, 0x73e7cca3, 0x34ace0ad, 0xc8ecb388, 0x5375c3aa, 0x9c194d87, 0x1b65246d,
    0xca000bcf, 0x8a0fb13d, 0x81b957b0, 0xac627bfb, 0xc0fe47e5, 0xf3db0ad8, 0x1c605c7d, 0x5f579884,
    0x63e079b5, 0x3d96f7cf, 0x3edd46e9, 0xc347c61e, 0x7b2b2a0e, 0x63dfcf51, 0x596781dd, 0x80304c4d,
    0xa66f8b47
};

// --- HELPER MATH FUNCTIONS ---

inline float positiveFraction(float x) {
    if (std::isnan(x) || std::isinf(x)) return 0;
    if (x < 0) x += static_cast<int>(1 - x);
    return x - static_cast<int>(x);
}

inline unsigned int rotateLeft(unsigned int x, const int b) {
    if (b > -32 && b < 32 && b != 0) {
        if (b < 0) x = (x >> (-b)) | (x << (32 + b));
        else       x = (x << b) | (x >> (32 - b));
    }
    return x;
}

// 1. SINEW (Period 1.0)
struct sin_wave : public exprtk::ifunction<double> {
    using exprtk::ifunction<double>::operator();
    sin_wave() : exprtk::ifunction<double>(1) {}
    inline double operator()(const double& x) {
        float fx = positiveFraction((float)x);
        return sinf(fx * F_2PI);
    }
};

// 2. SQUAREW
struct square_wave : public exprtk::ifunction<double> {
    using exprtk::ifunction<double>::operator();
    square_wave() : exprtk::ifunction<double>(1) {}
    inline double operator()(const double& x) {
        float fx = positiveFraction((float)x);
        return (fx >= 0.5f) ? -1.0 : 1.0;
    }
};

// 3. TRIANGLEW
struct triangle_wave : public exprtk::ifunction<double> {
    using exprtk::ifunction<double>::operator();
    triangle_wave() : exprtk::ifunction<double>(1) {}
    inline double operator()(const double& x) {
        float fx = positiveFraction((float)x);
        if (fx < 0.25f) return fx * 4.0f;
        else if (fx < 0.75f) return 2.0f - fx * 4.0f;
        else return fx * 4.0f - 4.0f;
    }
};

// 4. SAW (Standard saw wave)
struct saw_wave : public exprtk::ifunction<double> {
    using exprtk::ifunction<double>::operator();
    saw_wave() : exprtk::ifunction<double>(1) {}
    inline double operator()(const double& x) {
        float fx = positiveFraction((float)x);
        return 2.0f * fx - 1.0f;
    }
};

// 5. RANDSV (Deterministic Random)
struct randsv_func : public exprtk::ifunction<double> {
    using exprtk::ifunction<double>::operator();
    randsv_func() : exprtk::ifunction<double>(2) {}

    static const int data_size = 257;

    inline double operator()(const double& index, const double& seed) {
        float fidx = (float)index;
        int irseed = (int)seed;

        if (fidx < 0 || std::isnan(fidx)) return 0;

        const unsigned int xi = (unsigned int)fidx;
        const unsigned int si = irseed % data_size;
        const unsigned int sa = irseed / data_size;

        unsigned int res = rotateLeft(random_data[(xi + 23 * si + 1) % data_size] ^ random_data[(xi / data_size + sa) % data_size], sa % 31 + 1);
        res ^= rotateLeft(random_data[(3 * xi + si + 13) % data_size], (xi + 2 * si) % 32) ^ rotateLeft(random_data[(xi / data_size + 2 * sa) % data_size], xi % 31 + 1);

        return static_cast<int>(res) / (double)(1 << 31);
    }
};

// 6. CUSTOM CLAMP (LMMS style: min, val, max)
struct lmms_clamp : public exprtk::ifunction<double> {
    using exprtk::ifunction<double>::operator();
    lmms_clamp() : exprtk::ifunction<double>(3) {}
    inline double operator()(const double& minVal, const double& x, const double& maxVal) {
        if (x < minVal) return minVal;
        if (x > maxVal) return maxVal;
        return x;
    }
};

// --- DATA STRUCTURES ---
struct NoteEvent {
    double startTime;
    double duration;
    double frequency;
    QString noteName;
};

struct EffectData {
    QString name;
    double baseCutoff = 1000.0;

    QString getTransferFunction() const {
        QString cVal = QString::number((int)baseCutoff);
        QString n = name.toLower();
        if (n.contains("lowpass") || n.contains("lpf"))
            return QString("H(s) = 1 / (1 + s/" + cVal + ")");
        else if (n.contains("highpass") || n.contains("hpf"))
            return QString("H(s) = (s/" + cVal + ") / (1 + s/" + cVal + ")");
        else
            return QString("H(s) = 1");
    }
    bool isCompressor() const { return name.contains("compress", Qt::CaseInsensitive); }
};

struct XpressiveTrack {
    QString name;
    QString expressionO1;
    QString expressionW1;
    QString expressionW2;
    QString expressionW3;
    QVector<NoteEvent> notes;
    QVector<EffectData> effects;
};

// --- PARSING ---
inline double keyToFreq(int key) {
    return 440.0 * std::pow(2.0, (key - 69) / 12.0);
}

QString keyToNoteName(int key) {
    const char* names[] = {"C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"};
    int octave = (key / 12) - 1;
    int note = key % 12;
    return QString("%1%2").arg(names[note]).arg(octave);
}


double g_songDuration = 0.0;

QVector<XpressiveTrack> parseMMP(const QString &fileName, double &outBpm) {
    QVector<XpressiveTrack> tracks;
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly)) return tracks;

    QXmlStreamReader xml(&file);
    XpressiveTrack currentTrack;
    bool isXpressive = false;
    double currentBpm = 140.0;
    double currentPatternPos = 0.0;

    g_songDuration = 0.0; // Reset song duration

    while (!xml.atEnd()) {
        xml.readNext();
        if (xml.isStartElement()) {
            QString tagName = xml.name().toString();

            if (tagName == "head" && xml.attributes().hasAttribute("bpm")) {
                currentBpm = xml.attributes().value("bpm").toDouble();
                outBpm = currentBpm;
            }
            else if (tagName == "track") {
                if (!currentTrack.name.isEmpty() && isXpressive) tracks.append(currentTrack);
                currentTrack = XpressiveTrack();
                currentTrack.name = xml.attributes().value("name").toString();
                isXpressive = false;
                currentPatternPos = 0.0;
            }
            else if (tagName == u"instrument") {
                if (xml.attributes().value("name") == u"xpressive") isXpressive = true;
            }
            else if (tagName == "xpressive" && isXpressive) {
                currentTrack.expressionO1 = xml.attributes().value("O1").toString();
                currentTrack.expressionW1 = xml.attributes().value("w1").toString();
                currentTrack.expressionW2 = xml.attributes().value("w2").toString();
                currentTrack.expressionW3 = xml.attributes().value("w3").toString();
            }
            else if (tagName == "pattern") {
                if (xml.attributes().hasAttribute("pos"))
                    currentPatternPos = xml.attributes().value("pos").toDouble();
            }
            else if (tagName == "note" && isXpressive) {
                NoteEvent note;
                double relPos = xml.attributes().value("pos").toDouble();
                double len = xml.attributes().value("len").toDouble();

                if (len < 0) len = std::abs(len);

                int key = xml.attributes().value("key").toInt();

                double ticksPerBeat = 48.0; // Standard LMMS ticks per beat usually 48 or 192 depending on version, assuming 48 for base
                if (len == 192.0) ticksPerBeat = 48.0; // Adjustment based on typical XML values

                double secondsPerTick = 60.0 / (currentBpm * 48.0);

                note.startTime = (currentPatternPos + relPos) * secondsPerTick;
                note.duration = len * secondsPerTick;
                note.frequency = keyToFreq(key);
                note.noteName = keyToNoteName(key);
                currentTrack.notes.append(note);


                double noteEnd = note.startTime + note.duration;
                if (noteEnd > g_songDuration) g_songDuration = noteEnd;
            }
            else if (tagName == "effect" && isXpressive) {
                EffectData effect;
                effect.name = xml.attributes().value("name").toString();
                currentTrack.effects.append(effect);
            }
        }
        else if (xml.isEndElement() && xml.name() == u"pattern") {
            currentPatternPos = 0.0;
        }
    }
    if (!currentTrack.name.isEmpty() && isXpressive) tracks.append(currentTrack);


    g_songDuration += 1.0;

    return tracks;
}


class FormulaEvaluator {
public:
    exprtk::symbol_table<double> symbol_table;
    exprtk::expression<double> expression;
    exprtk::parser<double> parser;
    bool isValid = false;


    double t = 0.0, f = 440.0, srate = 44100.0, bnote = 57.0, v = 0.5;
    double w = 0.0, n = 0.0, p = 0.0;
    double A1 = 0.0, A2 = 0.0, A3 = 0.0, rel = 0.0, trel = 0.0;

    sin_wave f_sin; square_wave f_sqr; triangle_wave f_tri; saw_wave f_saw;
    randsv_func f_randsv; lmms_clamp f_clamp;


    struct dummy_integrate : public exprtk::ifunction<double> {
        using exprtk::ifunction<double>::operator();
        dummy_integrate() : exprtk::ifunction<double>(1) {}
        inline double operator()(const double& x) { return x; }
    } f_integrate;

    // Noise Generator
    struct noise_func : public exprtk::ifunction<double> {
        using exprtk::ifunction<double>::operator();
        noise_func() : exprtk::ifunction<double>(1) {}
        inline double operator()(const double& x) {
            return (positiveFraction(sin(x * 12.9898) * 43758.5453) * 2.0) - 1.0;
        }
    } f_noise;


    std::string cleanFormula(std::string expr) {
        if(expr.empty()) return "0";
        // 1. Fix "Digit attached to Function/Var" (e.g., "1saww" -> "1*saww")
        expr = std::regex_replace(expr, std::regex(R"((\d)([a-zA-Z_]))"), "$1*$2");
        // 2. Fix "Missing Operator between Groups" (e.g., ") (" -> ") + (")
        expr = std::regex_replace(expr, std::regex(R"((\))\s*(\())"), "$1 + $2");
        // 3. Fix "Missing Operator between End-Paren and Function"
        expr = std::regex_replace(expr, std::regex(R"((\))\s+([a-zA-Z_]))"), "$1 + $2");
        return expr;
    }

    FormulaEvaluator(QString o1, QString w1, QString w2, QString w3) {

        // Clean the Main Formula (O1)
        if (o1.isEmpty()) o1 = "sinew(t*f)";
        std::string s_o1 = cleanFormula(o1.toStdString());

        // Register Variables
        symbol_table.add_variable("t", t); symbol_table.add_variable("f", f);
        symbol_table.add_variable("srate", srate); symbol_table.add_variable("bnote", bnote);
        symbol_table.add_variable("v", v);
        symbol_table.add_variable("w", w); symbol_table.add_variable("n", n);
        symbol_table.add_variable("p", p);
        symbol_table.add_variable("A1", A1); symbol_table.add_variable("A2", A2);
        symbol_table.add_variable("A3", A3); symbol_table.add_variable("rel", rel);
        symbol_table.add_variable("trel", trel);
        symbol_table.add_constants();

        // Register Standard Functions
        symbol_table.add_function("sinew", f_sin); symbol_table.add_function("squarew", f_sqr);
        symbol_table.add_function("trianglew", f_tri); symbol_table.add_function("saww", f_saw);
        symbol_table.add_function("randsv", f_randsv); symbol_table.add_function("clamp", f_clamp);
        symbol_table.add_function("integrate", f_integrate);
        symbol_table.add_function("noise", f_noise); symbol_table.add_function("white_noise", f_noise);
        symbol_table.add_function("p_noise", f_noise);

        // Register Math Functions (Crucial for 'exp')
        symbol_table.add_function("exp", [](double x){ return std::exp(x); });
        symbol_table.add_function("abs", [](double x){ return std::abs(x); });
        symbol_table.add_function("sqrt", [](double x){ return std::sqrt(x); });
        symbol_table.add_function("log", [](double x){ return std::log(x); });
        symbol_table.add_function("pow", [](double b, double e){ return std::pow(b, e); });

        // Register Fallbacks
        symbol_table.add_function("moog_saw", f_saw);
        symbol_table.add_function("moogsaww", f_saw);
        symbol_table.add_function("rect", f_sqr);
        symbol_table.add_function("pulse", f_sqr);

        // SAFE MODE: Register W1/W2/W3 as standard waveforms
        symbol_table.add_function("W1", f_sin); symbol_table.add_function("w1", f_sin);
        symbol_table.add_function("W2", f_tri); symbol_table.add_function("w2", f_tri);
        symbol_table.add_function("W3", f_sqr); symbol_table.add_function("w3", f_sqr);

        expression.register_symbol_table(symbol_table);

        if (parser.compile(s_o1, expression)) {
            isValid = true;
        } else {
            qDebug() << "COMPILE ERROR:" << QString::fromStdString(s_o1);
        }
    }

    double evaluate(double noteTime, double freq) {
        if (!isValid) return 0.0;
        t = noteTime; f = freq;
        w = 2.0 * M_PI * freq; n = bnote;
        return expression.value();
    }
};

// --- WIDGET ---
class InstrumentWidget : public QWidget {
public:
    XpressiveTrack data;
    FormulaEvaluator* evaluator = nullptr;
    double currentTime = 0.0;

    InstrumentWidget(XpressiveTrack track, QWidget *parent = nullptr)
        : QWidget(parent), data(track) {
        setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        setMinimumSize(250, 180);
        setStyleSheet("background-color: #1e1e1e; border: 1px solid #444; border-radius: 4px;");

        evaluator = new FormulaEvaluator(data.expressionO1, data.expressionW1, data.expressionW2, data.expressionW3);
    }

    ~InstrumentWidget() {
        if (evaluator) delete evaluator;
    }

protected:
    void paintEvent(QPaintEvent *) override {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);
        int w = width();
        int h = height();

        // Active Notes Collection
        QVector<NoteEvent> activeNotes;
        QStringList activeNoteNames; // List to store names

        for (const auto &n : std::as_const(data.notes)) {
            // Check if note is playing (with a bit of release buffer)
            if (currentTime >= n.startTime && currentTime <= (n.startTime + n.duration + 0.3)) {
                activeNotes.append(n);
                // Detect if note is in "Attack/Sustain" phase for display
                if (currentTime <= n.startTime + n.duration) {
                    activeNoteNames.append(n.noteName);
                }
            }
        }

        // Layout
        int headerHeight = 25;
        int exprH = 35;
        int scopeTop = headerHeight + exprH + 5;
        int scopeH = h - scopeTop - 30;
        if (scopeH < 20) scopeH = 20;

        // Draw Header Name
        p.setPen(Qt::cyan);
        p.setFont(QFont("Arial", 12, QFont::Bold));
        QString title = data.name;

        // --- ADD NOTE NAMES TO TITLE ---
        if (!activeNoteNames.isEmpty()) {
            // Remove duplicates if any
            activeNoteNames.removeDuplicates();
            p.setPen(QColor(255, 200, 100)); // Orange-ish for notes
            title += "  [" + activeNoteNames.join(", ") + "]";
        }

        p.setPen(Qt::cyan);
        p.drawText(10, 20, data.name);

        if (!activeNoteNames.isEmpty()) {
            p.setPen(QColor(255, 200, 100));
            int nameWidth = QFontMetrics(p.font()).horizontalAdvance(data.name);
            p.drawText(10 + nameWidth + 10, 20, "[" + activeNoteNames.join(", ") + "]");
        }

        p.setPen(QColor(180, 180, 180));
        p.setFont(QFont("Consolas", 10));
        QRect exprRect(10, headerHeight, w - 20, exprH);
        p.drawText(exprRect, Qt::TextWordWrap, data.expressionO1);

        // Scope
        QRect scopeRect(10, scopeTop, w - 20, scopeH);
        drawScope(p, scopeRect, activeNotes);

        // Math Footer
        int mathY = h - 10;
        p.setPen(QColor(0, 200, 255));
        p.setFont(QFont("Times New Roman", 10, QFont::Normal, true));
        int mx = 10;
        for(const auto& eff : std::as_const(data.effects)) {
            if(eff.isCompressor()) continue;
            QString s = eff.getTransferFunction();
            p.drawText(mx, mathY, s);
            mx += (s.length()*7) + 20;
        }
    }

    void drawScope(QPainter &p, QRect rect, const QVector<NoteEvent> &notes) {
        QLinearGradient grad(rect.topLeft(), rect.bottomLeft());
        grad.setColorAt(0, QColor(20, 30, 20));
        grad.setColorAt(1, QColor(10, 20, 10));
        p.setBrush(grad);
        p.setPen(QColor(50, 80, 50));
        p.drawRoundedRect(rect, 6, 6);
        p.setPen(QColor(50, 150, 50, 50));
        p.drawLine(rect.left(), rect.center().y(), rect.right(), rect.center().y());

        // VISUAL ERROR INDICATION
        if (!evaluator || !evaluator->isValid) {
            p.setPen(Qt::red);
            p.drawText(rect, Qt::AlignCenter, "FORMULA ERROR");
            return;
        }

        if (notes.isEmpty()) return;

        p.setClipRect(rect);
        p.setPen(QPen(QColor(100, 255, 100), 2));
        QPainterPath path;

        double timeWindow = 0.05; // 50ms width
        double midY = rect.center().y();
        double amp = (rect.height() / 2.0) * 0.9;

        path.moveTo(rect.left(), midY);

        for (int x = 0; x <= rect.width(); x += 2) {
            double timeOffset = (x / (double)rect.width()) * timeWindow;
            double sampleTime = currentTime + timeOffset;
            double sampleValue = 0.0;

            for (const auto &n : notes) {
                double noteRelTime = sampleTime - n.startTime;

                // Skip if note hasn't started
                if (noteRelTime < 0) continue;

                // Evaluate Formula
                double val = evaluator->evaluate(noteRelTime, n.frequency);

                // If formula has no 'exp' (decay), apply a generic ADSR
                double env = 1.0;
                if (!data.expressionO1.contains("exp")) {
                    double releaseTime = 0.1; // 100ms release tail

                    if (noteRelTime < 0.01) {
                        // Attack (0 to 10ms)
                        env = noteRelTime / 0.01;
                    }
                    else if (noteRelTime > n.duration) {
                        // Release (Fade out after duration)
                        double timeSinceEnd = noteRelTime - n.duration;
                        if (timeSinceEnd < releaseTime) {
                            env = 1.0 - (timeSinceEnd / releaseTime);
                        } else {
                            env = 0.0;
                        }
                    }
                }

                // Hard cut only after the release tail is definitely gone
                if (noteRelTime > n.duration + 0.2) env = 0.0;

                sampleValue += (val * env);
            }

            // Simple mix normalisation
            if (notes.size() > 1) sampleValue /= std::sqrt((double)notes.size());

            // Clamp
            if (sampleValue > 1.0) sampleValue = 1.0;
            if (sampleValue < -1.0) sampleValue = -1.0;

            double plotY = midY - (sampleValue * amp);
            path.lineTo(rect.left() + x, plotY);
        }
        p.drawPath(path);
        p.setClipping(false);
    }
};

// --- MAIN WINDOW ---
class MainWindow : public QWidget {
public:
    QVector<InstrumentWidget*> instruments;
    QLabel *timeLabel;
    QLabel *bpmLabel;
    QPushButton *playBtn;
    QPushButton *loadBtn;
    QTimer *refreshTimer;
    QElapsedTimer wallClock;
    QWidget *gridContainer;
    QGridLayout *gridLayout;
    double currentSongTime = 0.0;
    double pausedTimeOffset = 0.0;
    bool isPlaying = false;
    double detectedBpm = 140.0;

    MainWindow() {
        QVBoxLayout *mainLayout = new QVBoxLayout(this);
        mainLayout->setContentsMargins(5,5,5,5);
        setStyleSheet("background-color: #121212; color: white;");

        QHBoxLayout *topBar = new QHBoxLayout();
        loadBtn = new QPushButton("LOAD FILE");
        loadBtn->setStyleSheet("background-color: #0066AA; border-radius: 4px; padding: 5px;");
        connect(loadBtn, &QPushButton::clicked, this, &MainWindow::openFile);

        playBtn = new QPushButton("START");
        playBtn->setStyleSheet("background-color: #00AA00; border-radius: 4px; padding: 5px;");
        connect(playBtn, &QPushButton::clicked, this, &MainWindow::togglePlay);

        timeLabel = new QLabel("00:00.000");
        timeLabel->setFont(QFont("Consolas", 16, QFont::Bold));
        timeLabel->setStyleSheet("color: #00FF00;");
        bpmLabel = new QLabel("BPM: --");

        topBar->addWidget(loadBtn); topBar->addWidget(playBtn);
        topBar->addWidget(timeLabel); topBar->addWidget(bpmLabel);
        topBar->addStretch();
        mainLayout->addLayout(topBar);

        gridContainer = new QWidget();
        gridLayout = new QGridLayout(gridContainer);
        gridLayout->setSpacing(5);
        mainLayout->addWidget(gridContainer, 1);

        refreshTimer = new QTimer(this);
        connect(refreshTimer, &QTimer::timeout, this, &MainWindow::updateLoop);
        refreshTimer->start(16);
    }

    void openFile() {
        QString fileName = QFileDialog::getOpenFileName(this, "Open LMMS Project", "", "LMMS Projects (*.mmp *.mmpz)");
        if (!fileName.isEmpty()) loadProject(fileName);
    }

    void loadProject(const QString &path) {
        if (isPlaying) togglePlay();
        currentSongTime = 0.0; pausedTimeOffset = 0.0;
        qDeleteAll(instruments); instruments.clear();
        QLayoutItem *item;
        while ((item = gridLayout->takeAt(0)) != nullptr) { delete item->widget(); delete item; }

        auto tracks = parseMMP(path, detectedBpm);
        if (tracks.isEmpty()) { QMessageBox::warning(this, "Error", "No tracks."); return; }
        bpmLabel->setText(QString("BPM: %1  Len: %2s").arg(detectedBpm).arg((int)g_songDuration));

        int row = 0, col = 0;
        // Fix for clazy loop warning: use std::as_const to prevent detach
        for (const auto &track : std::as_const(tracks)) {
            InstrumentWidget *w = new InstrumentWidget(track);
            instruments.append(w);
            gridLayout->addWidget(w, row, col++);
            if (col >= 2) { col = 0; row++; }
        }
    }

    void togglePlay() {
        isPlaying = !isPlaying;
        playBtn->setText(isPlaying ? "STOP" : "RESUME");
        if(isPlaying) wallClock.start();
        else pausedTimeOffset = currentSongTime;
    }

    void updateLoop() {
        if (isPlaying) {
            if (pausedTimeOffset == 0.0 && currentSongTime == 0.0) wallClock.restart();
            currentSongTime = pausedTimeOffset + (wallClock.nsecsElapsed() / 1e9);

            if (g_songDuration > 0.0 && currentSongTime > g_songDuration) {
                currentSongTime = 0.0;
                pausedTimeOffset = 0.0;
                wallClock.restart();
            }
        }
        int min = (int)(currentSongTime/60); int sec = (int)(currentSongTime)%60; int ms = (int)((currentSongTime-(int)currentSongTime)*1000);
        timeLabel->setText(QString("%1:%2.%3").arg(min,2,10,QChar('0')).arg(sec,2,10,QChar('0')).arg(ms,3,10,QChar('0')));
        for(auto *inst : instruments) { inst->currentTime = currentSongTime; inst->update(); }
    }
};

int main(int argc, char *argv[]) {
    QApplication a(argc, argv);
    MainWindow w;
    w.resize(1200, 800);
    w.show();
    return a.exec();
}


