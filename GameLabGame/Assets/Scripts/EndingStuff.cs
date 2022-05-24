using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UI;

public class EndingStuff : MonoBehaviour
{
    public AudioSource ads;
    public bool doneplayingendcutscene;
    public Image black;
    public int FileCounter = 0;
    public List<Texture2D> textures;
    public GameObject[] buttons;
    public GameObject content;
    public int selected;
    public GameObject polaroid;
    private const string TWITTER_ADDRESS = "http://twitter.com/intent/tweet";
    private const string TWEET_LANGUAGE = "en";

    // Start is called before the first frame update
    void Start()
    {
        Time.timeScale = 1;
        ImageStorage imgst = FindObjectOfType<ImageStorage>();
        textures = imgst.photos;
        createbuttons();
        ads.Play();
        StartCoroutine(soundend());
    }
    IEnumerator soundend()
    {
        yield return new WaitForSeconds(ads.clip.length - 2);
        float val2;
        for (val2 = 1; val2 > 0; val2 -= Time.deltaTime/2)
        {
            black.color = new Color(0, 0, 0, val2);
            if (doneplayingendcutscene)
            {
                black.color = new Color(0,0,0,0);
                StopCoroutine(soundend());
            }
            yield return new WaitForEndOfFrame();
        }
        black.color = new Color(0,0,0,0);
        doneplayingendcutscene = false;
    }

    // Update is called once per frame
    void Update()
    {
        if (Input.GetKeyDown(KeyCode.Space) && !doneplayingendcutscene)
        {
            doneplayingendcutscene = true;
        }
    }

    public void clickedon(int i)
    {
        Debug.Log("Clicked!");
        selected = i;
        for (int x = 0; x < buttons.Length; x++)
        {
            buttons[x].GetComponent<Image>().color = x == i ? Color.green : Color.white;
        }
    }

    void createbuttons()
    {
        buttons = new GameObject[textures.Count];
        for (int i = 0; i < textures.Count; i++)
        {
           GameObject g = Instantiate(polaroid, content.transform);
           int tempint = i;
           g.GetComponent<Button>().onClick.AddListener(() => clickedon(tempint));
           g.GetComponentInChildren<RawImage>().texture = textures[i];
           buttons[i] = g;
        }
    }

    public void quit()
    {
        SceneManager.LoadScene(0);
    }

    public void tweet()
    {
        if (selected == null) return; 
        var Bytes = textures[selected].EncodeToPNG();
    }

    public void delete(int i)
    {
        if (selected == null) return;
        Destroy(buttons[selected]);
    }
    public void Save()
    {
        if (selected == null) return;
        var Bytes = textures[selected].EncodeToPNG();
        string path = Environment.GetFolderPath(Environment.SpecialFolder.Desktop) +"/";
        path = path.Replace('\\','/');
        Debug.Log(path);
        while (File.Exists(path + "Photo" + FileCounter + ".png"))
            FileCounter++;
        File.WriteAllBytes( path + "Photo" + FileCounter + ".png", Bytes);
    }
}
