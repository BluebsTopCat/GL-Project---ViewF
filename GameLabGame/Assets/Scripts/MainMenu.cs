using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class MainMenu : MonoBehaviour
{
    public AudioSource select;
    public void startgame()
    {
        select.Play();
        SceneManager.LoadScene(1);
    }

    public void quitgame()
    {
        select.Play();
        Application.Quit();
    }

    public void settings()
    {
        select.Play();
    }
}
