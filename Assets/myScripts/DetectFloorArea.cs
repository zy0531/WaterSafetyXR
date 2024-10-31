using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using Meta.XR.Util;
using Meta.XR.MRUtilityKit;
using TMPro;

public class DetectFloorArea : MonoBehaviour
{

    bool IsFloorDetected = false;
    [SerializeField] GameObject _floorRepresentation;
    [SerializeField] GameObject _avatar;
    public float avatar_offsetY = -1.2f;

    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        if(!IsFloorDetected)
        {
            try
            {
                var surfaceType = MRUKAnchor.SceneLabels.FLOOR;
                var largestSurface = MRUK.Instance?.GetCurrentRoom()?.FindLargestSurface(surfaceType);
                if (largestSurface != null)
                {
                    Debug.Log("I find the FLOOR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
                    if (_floorRepresentation != null)
                    {
                        Vector3 anchorSize = largestSurface.VolumeBounds.HasValue
                            ? largestSurface.VolumeBounds.Value.size
                            : new Vector3(0, 0, 0.01f);
                        if (largestSurface.PlaneRect.HasValue)
                        {
                            anchorSize = new Vector3(largestSurface.PlaneRect.Value.x,
                                largestSurface.PlaneRect.Value.y, 0.01f);
                        }

                        _floorRepresentation.transform.localScale = anchorSize;
                        _floorRepresentation.transform.localPosition = largestSurface.transform.position;
                        _floorRepresentation.transform.localRotation = largestSurface.transform.rotation;

                        _floorRepresentation.SetActive(true);

                        if (_avatar != null)
                        {
                            _avatar.transform.localPosition = new Vector3(largestSurface.transform.position.x, largestSurface.transform.position.y + avatar_offsetY, largestSurface.transform.position.z);
                            _avatar.SetActive(true);
                        }

                        IsFloorDetected = true;

                        Debug.Log("I CREATE the FLOOR CUBE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");   
                    }

                }
            }
            catch (Exception e)
            {

            }
        }



        // Move avatar along wit the floor representation
        if (_floorRepresentation != null)
        {
            _avatar.transform.position = _floorRepresentation.transform.position + new Vector3(0f, avatar_offsetY, 0f);
        }
    }










}





