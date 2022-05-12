using System;
using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;
using UnityEngine.UI;

public class TextSender : MonoBehaviour
{
    public GameObject text;
    public Color you;
    public Color parent;

    public void sendtext(string s)
    {
       GameObject tb = Instantiate(text, this.transform);
       string[] stuff = s.Split(':');
       if (stuff[0] == "YOU") 
           tb.GetComponent<Image>().color = you;
       else 
           tb.GetComponent<Image>().color = parent;
       tb.GetComponentInChildren<TextMeshProUGUI>().text = stuff[1];
    }
}
