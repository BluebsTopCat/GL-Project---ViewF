using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Startcutscene : MonoBehaviour
{
    public Player p;
    public ContextMusic cm;

    public Image black;
    public AudioSource introsounds;
    // Start is called before the first frame update
    void Start()
    {
        StartCoroutine(startstuff());
    }

    IEnumerator startstuff()
    {
        introsounds.Play();
        StartCoroutine(soundend());
        yield return new WaitForSeconds(12);
        StartCoroutine(fadeins());
    }

    IEnumerator soundend()
    {
        yield return new WaitForSeconds(25);
        p.pause = false;
        float val2 = 1;
        for (val2 = 1; val2 > 0; val2 -= Time.deltaTime/2)
        {
            black.color = new Color(0, 0, 0, val2);
            yield return new WaitForEndOfFrame();
        }
        black.color = new Color(0,0,0,0);
    }

    IEnumerator fadeins()
    {
        float val = 1;
        for (val = 1; val > 0; val -= Time.deltaTime/5)
        {
            cm.mutedmanual = val;
            yield return new WaitForEndOfFrame();
        }
        cm.mutedmanual = 0;
    }

    public void skip()
    {
        black.color = new Color(0, 0, 0, 0);
        cm.mutedmanual = 0;
        p.pause = false;
        Destroy(gameObject);
    }

    // Update is called once per frame
    void Update()
    {
        if(Input.GetKeyDown(KeyCode.Space))
            skip();
    }
}
