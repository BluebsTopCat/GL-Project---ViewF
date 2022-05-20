using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MenuCycler : MonoBehaviour
{
    public int currentmenu = 0;

    public GameObject[] menus;

    // Update is called once per frame
    private void Start()
    {
        showrightmenu();
    }

    public void menuup()
    {
        currentmenu++;
        if (currentmenu > menus.Length - 1)
        {
            currentmenu = 0;
        }
        showrightmenu();
    }

    public void menudown()
    {
        currentmenu--;
        if (currentmenu < 0)
        {
            currentmenu = menus.Length - 1;
        }
        showrightmenu();
    }

    public void showrightmenu()
    {
        for(int i = 0; i< menus.Length; i++)
        {
            menus[i].SetActive(i == currentmenu);
        }
    }

}
