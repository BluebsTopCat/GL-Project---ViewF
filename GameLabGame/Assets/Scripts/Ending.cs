using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;
using UnityEngine.SceneManagement;

public class Ending : MonoBehaviour
{
    public bool confirmed;
    public TextMeshProUGUI text;
    public Menu m;

    public void yes()
    {
        if (!confirmed)
        {
            confirmed = true;
            text.text = "Am I sure?";
        }
        else
        {
            SceneManager.LoadScene(2);
        }
    }

    public void no()
    {
        confirmed = false;
        text.text = "Am I ready to go?";
        m.back();
        this.gameObject.SetActive(false);
    }
}
