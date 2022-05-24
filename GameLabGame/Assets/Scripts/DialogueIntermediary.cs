using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;
using Yarn.Unity;



public class DialogueIntermediary : MonoBehaviour
    {
        public TextMeshProUGUI Title;
        public TextMeshProUGUI Text;
        public List<TextMeshProUGUI> othertext;
        public UnityEngine.UI.Image Background;
        public List<UnityEngine.UI.Image> buttons;

        public List<NPCChunk> Npcs;

        // Start is called before the first frame update
        public void showthedialogue(Dialoguethingy dl)
        {
            string name = dl.title;
            string text = dl.text;
            int length = dl.size;
            
            NPCChunk npc = Npcs.Find(e => e.name == name.ToLower());

            Title.text = name;
            Title.color = npc.Textcolor;

            Text.text = text;
            Text.color = npc.Textcolor;

            foreach (TextMeshProUGUI tmp in othertext)
            {
                tmp.color = npc.Textcolor;
            }

            Background.color = npc.Bgcolor;
            foreach (UnityEngine.UI.Image b in buttons)
            {
                b.color = npc.Bgcolordark;
            }

            Text.maxVisibleCharacters = length;
        }
    }

    [System.Serializable]
    public class NPCChunk
    {
        public string name;
        public Color Textcolor;
        public Color Bgcolor;
        public Color Bgcolordark;

        public NPCChunk(string n, Color t, Color bg1, Color bg2)
        {
            name = n;
            Textcolor = t;
            Bgcolor = bg1;
            Bgcolordark = bg2;
        }
    }
