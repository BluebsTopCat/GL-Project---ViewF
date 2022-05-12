using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class trackcean : MonoBehaviour
{
    private GameObject player;
    // Start is called before the first frame update
    void Start()
    {
        player = GameObject.FindWithTag("Player");
    }

    // Update is called once per frame
    void Update()
    {
        this.transform.position = new Vector3(player.transform.position.x, this.transform.position.y, player.transform.position.z);
    }
}
