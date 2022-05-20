using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Soundeffectscamera : MonoBehaviour
{

    Player p;
    public AudioSource ads;
    public AudioClip takeout;
    public AudioClip putin;
    public AudioClip print;
    public AudioClip snap;
    public AudioClip beep;

    private void Start()
    {
        p = GameObject.FindObjectOfType<Player>();
    }

    public void pic()
    {
        p.snapped();
    }
    public void playto()
    {
        ads.PlayOneShot(takeout);
    }

    public void playpa()
    {
        ads.PlayOneShot(putin);
    }

    public void printpl()
    {
        ads.PlayOneShot(print);
    }

    public void playsnap()
    {
        ads.PlayOneShot(snap);
    }

    public void playbeep()
    {
        ads.PlayOneShot(beep);
    }
}
