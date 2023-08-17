package com.example.myapp.subActivities.type;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.databaseFiles.type.Type;

import java.util.List;

public class TypeDataAdapter extends ArrayAdapter<Type> {

    private final List<Type> typeList;

    //constructor for sport type spinner list adapter
    public TypeDataAdapter(@NonNull Context context, int resource, List<Type> typeList) {
        super(context, resource, typeList);
        this.typeList = typeList;
    }

    @NonNull
    @Override //get view for each sport type
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        //get text view ID for sport type name
        TextView label = (TextView) super.getView(position, convertView, parent);
        //set sport type name
        label.setText(typeList.get(position).getTypeName());
        //return sport type view
        return label;
    }

    @Override
    public View getDropDownView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        //get text view ID for sport type name
        TextView label = (TextView) super.getView(position, convertView, parent);
        //set sport type name
        label.setText(typeList.get(position).getTypeName());
        //return sport type view
        return label;
    }
}
